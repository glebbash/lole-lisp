import { panic } from 'panic-fn';

import { SExpr } from '../parser/parser';
import {
  expectArgsLength,
  expectArgsLengthAtLeast,
  expectList,
  expectNumber,
  expectString,
  expectSymbol,
  isList,
  isString,
  isSymbol,
} from './assertions';
import {
  LibLLVM,
  LLVMContext,
  LLVMIntPredicate,
  LLVMIRBuilder,
  LLVMModule,
  LLVMType,
  LLVMValue,
  loadLibLLVM,
} from './llvm-c';
import { getNumberValue, getStringValue } from './transformers';

type BasicCodegenContext = {
  llvm: LibLLVM;
  context: LLVMContext;
};

type CodegenContext = BasicCodegenContext & {
  builder: LLVMIRBuilder;
  module: LLVMModule;
  values: Record<string, LLVMValue>;
};

const VERIFICATION_ENABLED = false;

export function compile(
  exprs: SExpr[],
  outputIRFile: string,
  llvm = loadLibLLVM(),
) {
  const ctx: BasicCodegenContext = {
    llvm,
    context: llvm.contextCreate(),
  };

  const module = buildModule(ctx, exprs);
  llvm.printModuleToFile(module, outputIRFile);

  llvm.disposeModule(module);
  llvm.contextDispose(ctx.context);
}

function buildModule(
  parentCtx: BasicCodegenContext,
  exprs: SExpr[],
): LLVMModule {
  const { llvm } = parentCtx;

  const moduleName = 'main';
  const ctx: CodegenContext = {
    ...parentCtx,
    builder: llvm.createBuilderInContext(parentCtx.context),
    module: llvm.moduleCreateWithNameInContext(moduleName, parentCtx.context),
    values: {},
  };

  for (const expr of exprs) {
    buildValueInModuleContext(expr, ctx);
  }

  if (VERIFICATION_ENABLED) {
    const res = llvm.verifyModule(ctx.module);

    if (!res.ok) {
      console.error(res.message);
      panic(`Verifying module failed: ${moduleName}`);
    }
  }

  llvm.disposeBuilder(ctx.builder);

  return ctx.module;
}

function buildFn(
  command: string,
  args: SExpr[],
  moduleCtx: CodegenContext,
): LLVMValue {
  const { llvm } = moduleCtx;

  const [fnName, params, returnType, ...exprs] = expectArgsLengthAtLeast(
    3,
    args,
    command,
  );
  expectSymbol(fnName);
  expectList(params);
  expectSymbol(returnType);

  const paramInfos = params.map((paramInfo) => {
    expectList(paramInfo);

    if (paramInfo.length !== 2) {
      panic('Arguments in argument list must have name and type');
    }

    const [paramName, paramType] = paramInfo;
    expectSymbol(paramName);
    expectSymbol(paramType);

    return { name: paramName, type: getType(paramType, moduleCtx) };
  });

  const paramTypes = paramInfos.map((info) => info.type);
  const paramNames = paramInfos.map((info) => info.name);

  if (new Set(paramNames).size !== paramNames.length) {
    panic('Parameter names must be unique');
  }

  const fnType = llvm.functionType(getType(returnType, moduleCtx), paramTypes);

  const ctx: CodegenContext = {
    ...moduleCtx,
    values: { ...moduleCtx.values },
  };

  const fn = llvm.addFunction(moduleCtx.module, fnName, fnType);

  for (let index = 0; index < paramNames.length; index++) {
    const paramName = paramNames[index];
    ctx.values[paramName] = llvm.getParam(fn, index);
  }

  const entry = llvm.appendBasicBlockInContext(ctx.context, fn, 'entry');
  llvm.positionBuilderAtEnd(ctx.builder, entry);

  const values = exprs.map((expr) => buildValueInFunctionContext(expr, ctx));
  insertImplicitReturnOfLastValue(values, ctx);

  if (VERIFICATION_ENABLED && !llvm.verifyFunction(fn).ok) {
    panic(`Function verification failed: ${fnName}`);
  }

  return fn;
}

function buildValueInModuleContext(
  expr: SExpr,
  ctx: CodegenContext,
): LLVMValue {
  const [command, ...args] = expr;
  expectSymbol(command);

  switch (command) {
    case 'llvm/target-triple':
      return buildTargetTriple(command, args, ctx);
    case 'external-fn':
      return buildExternalFn(command, args, ctx);
    case 'fn':
      return buildFn(command, args, ctx);
    default:
      panic('Only functions and externs are allowed at top level');
  }
}

function buildValueInFunctionContext(
  expr: SExpr,
  ctx: CodegenContext,
): LLVMValue {
  if (!isList(expr)) {
    return buildValue(expr, ctx);
  }

  const [command, ...args] = expr;
  expectSymbol(command);

  switch (command) {
    case 'let':
      return buildLet(command, args, ctx);
    default:
      return buildValue(expr, ctx);
  }
}

function buildValue(expr: SExpr, ctx: CodegenContext): LLVMValue {
  if (isSymbol(expr)) {
    return buildConstantAccess(expr, ctx);
  }

  if (isString(expr)) {
    return buildString(expr, ctx);
  }

  return buildConstruct(expr, ctx);
}

function buildConstruct(expr: SExpr, ctx: CodegenContext): LLVMValue {
  expectList(expr);

  const [command, ...args] = expr;
  expectSymbol(command);

  switch (command) {
    case 'i8':
      return buildI8(command, args, ctx);
    case 'i32':
      return buildI32(command, args, ctx);
    case 'i64':
      return buildI64(command, args, ctx);
    case '+':
      return buildAdd(command, args, ctx);
    case '<':
      return buildLess(command, args, ctx);
    case 'nullptr':
      return buildNullPtr(command, args, ctx);
    case 'array':
      return buildArray(command, args, ctx);
    case 'get':
      return buildGet(command, args, ctx);
    case 'set':
      return buildSet(command, args, ctx);
    case 'i8':
      return buildI8(command, args, ctx);
    default:
      return buildFunctionCall(command, args, ctx);
  }
}

function buildArray(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const valueExprs = expectArgsLengthAtLeast(1, args, command);

  if (valueExprs.length === 0) {
    panic('Empty arrays are not allowed');
  }

  const values = valueExprs.map((expr) => buildValue(expr, ctx));
  const [firstValue, ...otherValues] = values;

  const elementType = llvm.typeOf(firstValue);
  const arrayType = llvm.arrayType(elementType, valueExprs.length);
  const array = llvm.buildAlloca(ctx.builder, arrayType);

  const zero = llvm.constInt(llvm.i32TypeInContext(ctx.context), 0);
  const firstElementPointer = llvm.buildGEP(ctx.builder, array, [zero, zero]);
  llvm.buildStore(ctx.builder, firstValue, firstElementPointer);

  let elementPointer = firstElementPointer;
  for (let index = 0; index < otherValues.length; index++) {
    const value = otherValues[index];
    elementPointer = llvm.buildGEP(ctx.builder, elementPointer, [
      llvm.constInt(llvm.i32TypeInContext(ctx.context), 1),
    ]);
    llvm.buildStore(ctx.builder, value, elementPointer);
  }

  return firstElementPointer;
}

function buildAdd(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [lhs, rhs] = expectArgsLength(2, args, command);

  return llvm.buildAdd(ctx.builder, buildValue(lhs, ctx), buildValue(rhs, ctx));
}

function buildLess(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [lhs, rhs] = expectArgsLength(2, args, command);

  const res = llvm.buildICmp(
    ctx.builder,
    LLVMIntPredicate.LLVMIntSLT,
    buildValue(lhs, ctx),
    buildValue(rhs, ctx),
  );

  return res;
}

function buildNullPtr(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [typeName] = expectArgsLength(1, args, command);
  expectSymbol(typeName);

  const type = getType(typeName, ctx);
  return llvm.constPointerNull(type);
}

function buildI64(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [value] = expectArgsLength(1, args, command);
  expectNumber(value);

  const i64Value = getNumberValue(value);

  return llvm.constInt(llvm.i64TypeInContext(ctx.context), i64Value);
}

function buildI32(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [value] = expectArgsLength(1, args, command);
  expectNumber(value);

  const i32Value = getNumberValue(value);

  return llvm.constInt(llvm.i32TypeInContext(ctx.context), i32Value);
}

function buildI8(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [value] = expectArgsLength(1, args, command);
  expectNumber(value);

  const i8Value = getNumberValue(value);

  return llvm.constInt(llvm.i32TypeInContext(ctx.context), i8Value);
}

function buildLet(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const [name, expr] = expectArgsLength(2, args, command);
  expectSymbol(name);

  if (ctx.values[name]) {
    panic(`Constant ${name} is already defined`);
  }

  const value = buildValue(expr, ctx);
  ctx.values[name] = value;
  return value;
}

function buildExternalFn(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [fnName, argTypes, returnType] = expectArgsLength(3, args, command);
  expectSymbol(fnName);
  expectList(argTypes);
  expectSymbol(returnType);

  llvm.addFunction(
    ctx.module,
    fnName,
    llvm.functionType(
      getType(returnType, ctx),
      argTypes.map((argType) => {
        expectSymbol(argType);

        return getType(argType, ctx);
      }),
    ),
  );

  return buildVoid(ctx);
}

function buildTargetTriple(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [targetTriple] = expectArgsLength(1, args, command);
  expectString(targetTriple);

  llvm.setTarget(ctx.module, getStringValue(targetTriple));

  return buildVoid(ctx);
}

function insertImplicitReturnOfLastValue(
  values: LLVMValue[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const lastValue = values.at(-1);

  const returnValue = lastValue ?? buildVoid(ctx);

  // TODO: add check for return type (`returnValue.getType()` throws 'TypeError: Illegal invocation')
  // if (!Type.isSameType(ctx.fn.getReturnType(), returnValue.getType())) {
  //   panic(
  //     `Function ${ctx.fn.getName()} must return ${ctx.fn.getReturnType()} but ${returnValue.getType()} was found`,
  //   );
  // }

  return llvm.buildRet(ctx.builder, returnValue);
}

function buildGet(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [sourcePtrExpr, ...indices] = expectArgsLengthAtLeast(2, args, command);

  const sourcePointer = buildValue(sourcePtrExpr, ctx);
  const indicesValues = indices.map((index) => buildValue(index, ctx));

  const elementPointer = llvm.buildGEP(
    ctx.builder,
    sourcePointer,
    indicesValues,
  );

  return llvm.buildLoad(ctx.builder, elementPointer);
}

function buildSet(
  command: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const [sourcePtrExpr, ...indicesAndValue] = expectArgsLengthAtLeast(
    3,
    args,
    command,
  );

  const sourcePointer = buildValue(sourcePtrExpr, ctx);
  const value = buildValue(indicesAndValue[indicesAndValue.length - 1], ctx);
  const indicesValues = indicesAndValue
    .slice(0, -1)
    .map((index) => buildValue(index, ctx));

  const elementPointer = llvm.buildGEP(
    ctx.builder,
    sourcePointer,
    indicesValues,
  );

  return llvm.buildStore(ctx.builder, value, elementPointer);
}

function buildConstantAccess(name: string, ctx: CodegenContext): LLVMValue {
  const constant = ctx.values[name];

  if (!constant) {
    panic(`Constant is not defined ${name}`);
  }

  return constant;
}

function buildString(expr: string, ctx: CodegenContext): LLVMValue {
  const { llvm } = ctx;

  return llvm.buildGlobalStringPtr(ctx.builder, getStringValue(expr));
}

function buildFunctionCall(
  fnName: string,
  args: SExpr[],
  ctx: CodegenContext,
): LLVMValue {
  const { llvm } = ctx;

  const callee = llvm.getNamedFunction(ctx.module, fnName);

  if (callee.value.isNull()) {
    panic(`Function ${fnName} is not defined`);
  }

  return llvm.buildCall(
    ctx.builder,
    callee,
    args.map((arg) => buildValue(arg, ctx)),
  );
}

function buildVoid(ctx: CodegenContext): LLVMValue {
  const { llvm } = ctx;

  return llvm.getUndef(llvm.voidTypeInContext(ctx.context));
}

function getType(typeName: string, ctx: CodegenContext): LLVMType {
  const { llvm } = ctx;

  switch (typeName) {
    case 'i1':
      return llvm.i1TypeInContext(ctx.context);
    case 'i32':
      return llvm.i32TypeInContext(ctx.context);
    case 'i64':
      return llvm.i64TypeInContext(ctx.context);
    case '&i8':
      return llvm.pointerType(llvm.i8TypeInContext(ctx.context));
    case '&&i8':
      return llvm.pointerType(
        llvm.pointerType(llvm.i8TypeInContext(ctx.context)),
      );
    case 'void':
      return llvm.voidTypeInContext(ctx.context);
    default:
      panic(`Unknown type: ${typeName}`);
  }
}
