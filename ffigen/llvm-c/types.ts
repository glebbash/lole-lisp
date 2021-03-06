// deno-lint-ignore-file
import { Opaque, Pointer, FnPointer, StructPointer } from "./safe-ffi.ts";

export namespace LLVM {
  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L28 */
  export type Bool = Opaque<number, "Bool">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L48 */
  export type MemoryBufferRef = Pointer<"MemoryBufferRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L53 */
  export type ContextRef = Pointer<"ContextRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L61 */
  export type ModuleRef = Pointer<"ModuleRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L68 */
  export type TypeRef = Pointer<"TypeRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L75 */
  export type ValueRef = Pointer<"ValueRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L82 */
  export type BasicBlockRef = Pointer<"BasicBlockRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L89 */
  export type MetadataRef = Pointer<"MetadataRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L96 */
  export type NamedMDNodeRef = Pointer<"NamedMDNodeRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L103 */
  export type ValueMetadataEntry = StructPointer<"ValueMetadataEntry">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L110 */
  export type BuilderRef = Pointer<"BuilderRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L117 */
  export type DIBuilderRef = Pointer<"DIBuilderRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L124 */
  export type ModuleProviderRef = Pointer<"ModuleProviderRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L127 */
  export type PassManagerRef = Pointer<"PassManagerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L130 */
  export type PassRegistryRef = Pointer<"PassRegistryRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L136 */
  export type UseRef = Pointer<"UseRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L143 */
  export type AttributeRef = Pointer<"AttributeRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L148 */
  export type DiagnosticInfoRef = Pointer<"DiagnosticInfoRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L153 */
  export type ComdatRef = Pointer<"ComdatRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L158 */
  export type ModuleFlagEntry = StructPointer<"ModuleFlagEntry">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L163 */
  export type JITEventListenerRef = Pointer<"JITEventListenerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Types.h#L168 */
  export type BinaryRef = Pointer<"BinaryRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L37 */
  export type TargetDataRef = Pointer<"TargetDataRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L38 */
  export type TargetLibraryInfoRef = Pointer<"TargetLibraryInfoRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L34 */
  export type TargetMachineRef = Pointer<"TargetMachineRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L35 */
  export type TargetRef = Pointer<"TargetRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L39 */
  export type GenericValueRef = Pointer<"GenericValueRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L40 */
  export type ExecutionEngineRef = Pointer<"ExecutionEngineRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L41 */
  export type MCJITMemoryManagerRef = Pointer<"MCJITMemoryManagerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L159 */
  export type MemoryManagerAllocateCodeSectionCallback = FnPointer<"MemoryManagerAllocateCodeSectionCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L162 */
  export type MemoryManagerAllocateDataSectionCallback = FnPointer<"MemoryManagerAllocateDataSectionCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L165 */
  export type MemoryManagerFinalizeMemoryCallback = FnPointer<"MemoryManagerFinalizeMemoryCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L167 */
  export type MemoryManagerDestroyCallback = FnPointer<"MemoryManagerDestroyCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DisassemblerTypes.h#L29 */
  export type DisasmContextRef = Pointer<"DisasmContextRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DisassemblerTypes.h#L48 */
  export type OpInfoCallback = FnPointer<"OpInfoCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DisassemblerTypes.h#L118 */
  export type SymbolLookupCallback = FnPointer<"SymbolLookupCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L174 */
  export type MetadataKind = Opaque<number, "MetadataKind">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L179 */
  export type DWARFTypeEncoding = Opaque<number, "DWARFTypeEncoding">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L33 */
  export type ErrorRef = Pointer<"ErrorRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L38 */
  export type ErrorTypeId = Pointer<"ErrorTypeId">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L46 */
  export type OrcJITTargetAddress = Opaque<bigint, "OrcJITTargetAddress">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L51 */
  export type OrcExecutorAddress = Opaque<bigint, "OrcExecutorAddress">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L66 */
  export type JITSymbolTargetFlags = Opaque<number, "JITSymbolTargetFlags">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L74 */
  export type JITSymbolFlags = StructPointer<"JITSymbolFlags">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L82 */
  export type JITEvaluatedSymbol = StructPointer<"JITEvaluatedSymbol">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L87 */
  export type OrcExecutionSessionRef = Pointer<"OrcExecutionSessionRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L92 */
  export type OrcErrorReporterFunction = FnPointer<"OrcErrorReporterFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L97 */
  export type OrcSymbolStringPoolRef = Pointer<"OrcSymbolStringPoolRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L103 */
  export type OrcSymbolStringPoolEntryRef = Pointer<"OrcSymbolStringPoolEntryRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L111 */
  export type OrcCSymbolFlagsMapPair = StructPointer<"OrcCSymbolFlagsMapPair">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L117 */
  export type OrcCSymbolFlagsMapPairs = Pointer<"OrcCSymbolFlagsMapPairs">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L125 */
  export type JITCSymbolMapPair = StructPointer<"JITCSymbolMapPair">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L131 */
  export type OrcCSymbolMapPairs = Pointer<"OrcCSymbolMapPairs">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L139 */
  export type OrcCSymbolAliasMapEntry = StructPointer<"OrcCSymbolAliasMapEntry">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L147 */
  export type OrcCSymbolAliasMapPair = StructPointer<"OrcCSymbolAliasMapPair">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L153 */
  export type OrcCSymbolAliasMapPairs = Pointer<"OrcCSymbolAliasMapPairs">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L158 */
  export type OrcJITDylibRef = Pointer<"OrcJITDylibRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L167 */
  export type OrcCSymbolsList = StructPointer<"OrcCSymbolsList">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L175 */
  export type OrcCDependenceMapPair = StructPointer<"OrcCDependenceMapPair">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L181 */
  export type OrcCDependenceMapPairs = Pointer<"OrcCDependenceMapPairs">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L220 */
  export type OrcCLookupSetElement = StructPointer<"OrcCLookupSetElement">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L233 */
  export type OrcCLookupSet = Pointer<"OrcCLookupSet">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L238 */
  export type OrcMaterializationUnitRef = Pointer<"OrcMaterializationUnitRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L246 */
  export type OrcMaterializationResponsibilityRef = Pointer<"OrcMaterializationResponsibilityRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L258 */
  export type OrcMaterializationUnitMaterializeFunction = FnPointer<"OrcMaterializationUnitMaterializeFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L267 */
  export type OrcMaterializationUnitDiscardFunction = FnPointer<"OrcMaterializationUnitDiscardFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L277 */
  export type OrcMaterializationUnitDestroyFunction = FnPointer<"OrcMaterializationUnitDestroyFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L282 */
  export type OrcResourceTrackerRef = Pointer<"OrcResourceTrackerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L288 */
  export type OrcDefinitionGeneratorRef = Pointer<"OrcDefinitionGeneratorRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L302 */
  export type OrcLookupStateRef = Pointer<"OrcLookupStateRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L337 */
  export type OrcCAPIDefinitionGeneratorTryToGenerateFunction = FnPointer<"OrcCAPIDefinitionGeneratorTryToGenerateFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L346 */
  export type OrcSymbolPredicate = FnPointer<"OrcSymbolPredicate">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L352 */
  export type OrcThreadSafeContextRef = Pointer<"OrcThreadSafeContextRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L357 */
  export type OrcThreadSafeModuleRef = Pointer<"OrcThreadSafeModuleRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L363 */
  export type OrcGenericIRModuleOperationFunction = FnPointer<"OrcGenericIRModuleOperationFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L370 */
  export type OrcJITTargetMachineBuilderRef = Pointer<"OrcJITTargetMachineBuilderRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L375 */
  export type OrcObjectLayerRef = Pointer<"OrcObjectLayerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L380 */
  export type OrcObjectLinkingLayerRef = Pointer<"OrcObjectLinkingLayerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L385 */
  export type OrcIRTransformLayerRef = Pointer<"OrcIRTransformLayerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L402 */
  export type OrcIRTransformLayerTransformFunction = FnPointer<"OrcIRTransformLayerTransformFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L410 */
  export type OrcObjectTransformLayerRef = Pointer<"OrcObjectTransformLayerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L425 */
  export type OrcObjectTransformLayerTransformFunction = FnPointer<"OrcObjectTransformLayerTransformFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L432 */
  export type OrcIndirectStubsManagerRef = Pointer<"OrcIndirectStubsManagerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L438 */
  export type OrcLazyCallThroughManagerRef = Pointer<"OrcLazyCallThroughManagerRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L446 */
  export type OrcDumpObjectsRef = Pointer<"OrcDumpObjectsRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L20 */
  export type PassManagerBuilderRef = Pointer<"PassManagerBuilderRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L38 */
  export type PassBuilderOptionsRef = Pointer<"PassBuilderOptionsRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L57 */
  export type RemarkStringRef = Pointer<"RemarkStringRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L78 */
  export type RemarkDebugLocRef = Pointer<"RemarkDebugLocRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L109 */
  export type RemarkArgRef = Pointer<"RemarkArgRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L140 */
  export type RemarkEntryRef = Pointer<"RemarkEntryRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L230 */
  export type RemarkParserRef = Pointer<"RemarkParserRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ErrorHandling.h#L27 */
  export type FatalErrorHandler = FnPointer<"FatalErrorHandler">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L469 */
  export type AttributeIndex = Opaque<number, "AttributeIndex">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L499 */
  export type DiagnosticHandler = FnPointer<"DiagnosticHandler">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L500 */
  export type YieldCallback = FnPointer<"YieldCallback">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L36 */
  export type SectionIteratorRef = Pointer<"SectionIteratorRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L37 */
  export type SymbolIteratorRef = Pointer<"SymbolIteratorRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L38 */
  export type RelocationIteratorRef = Pointer<"RelocationIteratorRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L205 */
  export type ObjectFileRef = Pointer<"ObjectFileRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L55 */
  export type OrcLLJITBuilderObjectLinkingLayerCreatorFunction = FnPointer<"OrcLLJITBuilderObjectLinkingLayerCreatorFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L61 */
  export type OrcLLJITBuilderRef = Pointer<"OrcLLJITBuilderRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L66 */
  export type OrcLLJITRef = Pointer<"OrcLLJITRef">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Analysis.h#L34 */
  export enum VerifierFailureAction {
    LLVMAbortProcessAction = 0,
    LLVMPrintMessageAction = 1,
    LLVMReturnStatusAction = 2,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L35 */
  export enum ByteOrdering {
    LLVMBigEndian = 0,
    LLVMLittleEndian = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L37 */
  export enum CodeGenOptLevel {
    LLVMCodeGenLevelNone = 0,
    LLVMCodeGenLevelLess = 1,
    LLVMCodeGenLevelDefault = 2,
    LLVMCodeGenLevelAggressive = 3,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L44 */
  export enum RelocMode {
    LLVMRelocDefault = 0,
    LLVMRelocStatic = 1,
    LLVMRelocPIC = 2,
    LLVMRelocDynamicNoPic = 3,
    LLVMRelocROPI = 4,
    LLVMRelocRWPI = 5,
    LLVMRelocROPI_RWPI = 6,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L54 */
  export enum CodeModel {
    LLVMCodeModelDefault = 0,
    LLVMCodeModelJITDefault = 1,
    LLVMCodeModelTiny = 2,
    LLVMCodeModelSmall = 3,
    LLVMCodeModelKernel = 4,
    LLVMCodeModelMedium = 5,
    LLVMCodeModelLarge = 6,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L64 */
  export enum CodeGenFileType {
    LLVMAssemblyFile = 0,
    LLVMObjectFile = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L34 */
  export enum DIFlags {
    LLVMDIFlagZero = 0,
    LLVMDIFlagPrivate = 1,
    LLVMDIFlagProtected = 2,
    LLVMDIFlagPublic = 3,
    LLVMDIFlagFwdDecl = 4,
    LLVMDIFlagAppleBlock = 8,
    LLVMDIFlagReservedBit4 = 16,
    LLVMDIFlagVirtual = 32,
    LLVMDIFlagArtificial = 64,
    LLVMDIFlagExplicit = 128,
    LLVMDIFlagPrototyped = 256,
    LLVMDIFlagObjcClassComplete = 512,
    LLVMDIFlagObjectPointer = 1024,
    LLVMDIFlagVector = 2048,
    LLVMDIFlagStaticMember = 4096,
    LLVMDIFlagLValueReference = 8192,
    LLVMDIFlagRValueReference = 16384,
    LLVMDIFlagReserved = 32768,
    LLVMDIFlagSingleInheritance = 65536,
    LLVMDIFlagMultipleInheritance = 131072,
    LLVMDIFlagVirtualInheritance = 196608,
    LLVMDIFlagIntroducedVirtual = 262144,
    LLVMDIFlagBitField = 524288,
    LLVMDIFlagNoReturn = 1048576,
    LLVMDIFlagTypePassByValue = 4194304,
    LLVMDIFlagTypePassByReference = 8388608,
    LLVMDIFlagEnumClass = 16777216,
    LLVMDIFlagFixedEnum = 16777216,
    LLVMDIFlagThunk = 33554432,
    LLVMDIFlagNonTrivial = 67108864,
    LLVMDIFlagBigEndian = 134217728,
    LLVMDIFlagLittleEndian = 268435456,
    LLVMDIFlagIndirectVirtualBase = 36,
    LLVMDIFlagAccessibility = 3,
    LLVMDIFlagPtrToMemberRep = 196608,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L78 */
  export enum DWARFSourceLanguage {
    LLVMDWARFSourceLanguageC89 = 0,
    LLVMDWARFSourceLanguageC = 1,
    LLVMDWARFSourceLanguageAda83 = 2,
    LLVMDWARFSourceLanguageC_plus_plus = 3,
    LLVMDWARFSourceLanguageCobol74 = 4,
    LLVMDWARFSourceLanguageCobol85 = 5,
    LLVMDWARFSourceLanguageFortran77 = 6,
    LLVMDWARFSourceLanguageFortran90 = 7,
    LLVMDWARFSourceLanguagePascal83 = 8,
    LLVMDWARFSourceLanguageModula2 = 9,
    LLVMDWARFSourceLanguageJava = 10,
    LLVMDWARFSourceLanguageC99 = 11,
    LLVMDWARFSourceLanguageAda95 = 12,
    LLVMDWARFSourceLanguageFortran95 = 13,
    LLVMDWARFSourceLanguagePLI = 14,
    LLVMDWARFSourceLanguageObjC = 15,
    LLVMDWARFSourceLanguageObjC_plus_plus = 16,
    LLVMDWARFSourceLanguageUPC = 17,
    LLVMDWARFSourceLanguageD = 18,
    LLVMDWARFSourceLanguagePython = 19,
    LLVMDWARFSourceLanguageOpenCL = 20,
    LLVMDWARFSourceLanguageGo = 21,
    LLVMDWARFSourceLanguageModula3 = 22,
    LLVMDWARFSourceLanguageHaskell = 23,
    LLVMDWARFSourceLanguageC_plus_plus_03 = 24,
    LLVMDWARFSourceLanguageC_plus_plus_11 = 25,
    LLVMDWARFSourceLanguageOCaml = 26,
    LLVMDWARFSourceLanguageRust = 27,
    LLVMDWARFSourceLanguageC11 = 28,
    LLVMDWARFSourceLanguageSwift = 29,
    LLVMDWARFSourceLanguageJulia = 30,
    LLVMDWARFSourceLanguageDylan = 31,
    LLVMDWARFSourceLanguageC_plus_plus_14 = 32,
    LLVMDWARFSourceLanguageFortran03 = 33,
    LLVMDWARFSourceLanguageFortran08 = 34,
    LLVMDWARFSourceLanguageRenderScript = 35,
    LLVMDWARFSourceLanguageBLISS = 36,
    LLVMDWARFSourceLanguageMips_Assembler = 37,
    LLVMDWARFSourceLanguageGOOGLE_RenderScript = 38,
    LLVMDWARFSourceLanguageBORLAND_Delphi = 39,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L128 */
  export enum DWARFEmissionKind {
    LLVMDWARFEmissionNone = 0,
    LLVMDWARFEmissionFull = 1,
    LLVMDWARFEmissionLineTablesOnly = 2,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L186 */
  export enum DWARFMacinfoRecordType {
    LLVMDWARFMacinfoRecordTypeDefine = 1,
    LLVMDWARFMacinfoRecordTypeMacro = 2,
    LLVMDWARFMacinfoRecordTypeStartFile = 3,
    LLVMDWARFMacinfoRecordTypeEndFile = 4,
    LLVMDWARFMacinfoRecordTypeVendorExt = 255,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L56 */
  export enum JITSymbolGenericFlags {
    LLVMJITSymbolGenericFlagsExported = 1,
    LLVMJITSymbolGenericFlagsWeak = 2,
    LLVMJITSymbolGenericFlagsCallable = 4,
    LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly = 8,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L189 */
  export enum OrcLookupKind {
    LLVMOrcLookupKindStatic = 0,
    LLVMOrcLookupKindDLSym = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L200 */
  export enum OrcJITDylibLookupFlags {
    LLVMOrcJITDylibLookupFlagsMatchExportedSymbolsOnly = 0,
    LLVMOrcJITDylibLookupFlagsMatchAllSymbols = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L209 */
  export enum OrcSymbolLookupFlags {
    LLVMOrcSymbolLookupFlagsRequiredSymbol = 0,
    LLVMOrcSymbolLookupFlagsWeaklyReferencedSymbol = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Linker.h#L30 */
  export enum LinkerMode {
    LLVMLinkerDestroySource = 0,
    LLVMLinkerPreserveSource_Removed = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L41 */
  export enum RemarkType {
    LLVMRemarkTypeUnknown = 0,
    LLVMRemarkTypePassed = 1,
    LLVMRemarkTypeMissed = 2,
    LLVMRemarkTypeAnalysis = 3,
    LLVMRemarkTypeAnalysisFPCommute = 4,
    LLVMRemarkTypeAnalysisAliasing = 5,
    LLVMRemarkTypeFailure = 6,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L60 */
  export enum Opcode {
    LLVMRet = 1,
    LLVMBr = 2,
    LLVMSwitch = 3,
    LLVMIndirectBr = 4,
    LLVMInvoke = 5,
    LLVMUnreachable = 7,
    LLVMCallBr = 67,
    LLVMFNeg = 66,
    LLVMAdd = 8,
    LLVMFAdd = 9,
    LLVMSub = 10,
    LLVMFSub = 11,
    LLVMMul = 12,
    LLVMFMul = 13,
    LLVMUDiv = 14,
    LLVMSDiv = 15,
    LLVMFDiv = 16,
    LLVMURem = 17,
    LLVMSRem = 18,
    LLVMFRem = 19,
    LLVMShl = 20,
    LLVMLShr = 21,
    LLVMAShr = 22,
    LLVMAnd = 23,
    LLVMOr = 24,
    LLVMXor = 25,
    LLVMAlloca = 26,
    LLVMLoad = 27,
    LLVMStore = 28,
    LLVMGetElementPtr = 29,
    LLVMTrunc = 30,
    LLVMZExt = 31,
    LLVMSExt = 32,
    LLVMFPToUI = 33,
    LLVMFPToSI = 34,
    LLVMUIToFP = 35,
    LLVMSIToFP = 36,
    LLVMFPTrunc = 37,
    LLVMFPExt = 38,
    LLVMPtrToInt = 39,
    LLVMIntToPtr = 40,
    LLVMBitCast = 41,
    LLVMAddrSpaceCast = 60,
    LLVMICmp = 42,
    LLVMFCmp = 43,
    LLVMPHI = 44,
    LLVMCall = 45,
    LLVMSelect = 46,
    LLVMUserOp1 = 47,
    LLVMUserOp2 = 48,
    LLVMVAArg = 49,
    LLVMExtractElement = 50,
    LLVMInsertElement = 51,
    LLVMShuffleVector = 52,
    LLVMExtractValue = 53,
    LLVMInsertValue = 54,
    LLVMFreeze = 68,
    LLVMFence = 55,
    LLVMAtomicCmpXchg = 56,
    LLVMAtomicRMW = 57,
    LLVMResume = 58,
    LLVMLandingPad = 59,
    LLVMCleanupRet = 61,
    LLVMCatchRet = 62,
    LLVMCatchPad = 63,
    LLVMCleanupPad = 64,
    LLVMCatchSwitch = 65,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L148 */
  export enum TypeKind {
    LLVMVoidTypeKind = 0,
    LLVMHalfTypeKind = 1,
    LLVMFloatTypeKind = 2,
    LLVMDoubleTypeKind = 3,
    LLVMX86_FP80TypeKind = 4,
    LLVMFP128TypeKind = 5,
    LLVMPPC_FP128TypeKind = 6,
    LLVMLabelTypeKind = 7,
    LLVMIntegerTypeKind = 8,
    LLVMFunctionTypeKind = 9,
    LLVMStructTypeKind = 10,
    LLVMArrayTypeKind = 11,
    LLVMPointerTypeKind = 12,
    LLVMVectorTypeKind = 13,
    LLVMMetadataTypeKind = 14,
    LLVMX86_MMXTypeKind = 15,
    LLVMTokenTypeKind = 16,
    LLVMScalableVectorTypeKind = 17,
    LLVMBFloatTypeKind = 18,
    LLVMX86_AMXTypeKind = 19,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L171 */
  export enum Linkage {
    LLVMExternalLinkage = 0,
    LLVMAvailableExternallyLinkage = 1,
    LLVMLinkOnceAnyLinkage = 2,
    LLVMLinkOnceODRLinkage = 3,
    LLVMLinkOnceODRAutoHideLinkage = 4,
    LLVMWeakAnyLinkage = 5,
    LLVMWeakODRLinkage = 6,
    LLVMAppendingLinkage = 7,
    LLVMInternalLinkage = 8,
    LLVMPrivateLinkage = 9,
    LLVMDLLImportLinkage = 10,
    LLVMDLLExportLinkage = 11,
    LLVMExternalWeakLinkage = 12,
    LLVMGhostLinkage = 13,
    LLVMCommonLinkage = 14,
    LLVMLinkerPrivateLinkage = 15,
    LLVMLinkerPrivateWeakLinkage = 16,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L194 */
  export enum Visibility {
    LLVMDefaultVisibility = 0,
    LLVMHiddenVisibility = 1,
    LLVMProtectedVisibility = 2,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L200 */
  export enum UnnamedAddr {
    LLVMNoUnnamedAddr = 0,
    LLVMLocalUnnamedAddr = 1,
    LLVMGlobalUnnamedAddr = 2,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L206 */
  export enum DLLStorageClass {
    LLVMDefaultStorageClass = 0,
    LLVMDLLImportStorageClass = 1,
    LLVMDLLExportStorageClass = 2,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L212 */
  export enum CallConv {
    LLVMCCallConv = 0,
    LLVMFastCallConv = 8,
    LLVMColdCallConv = 9,
    LLVMGHCCallConv = 10,
    LLVMHiPECallConv = 11,
    LLVMWebKitJSCallConv = 12,
    LLVMAnyRegCallConv = 13,
    LLVMPreserveMostCallConv = 14,
    LLVMPreserveAllCallConv = 15,
    LLVMSwiftCallConv = 16,
    LLVMCXXFASTTLSCallConv = 17,
    LLVMX86StdcallCallConv = 64,
    LLVMX86FastcallCallConv = 65,
    LLVMARMAPCSCallConv = 66,
    LLVMARMAAPCSCallConv = 67,
    LLVMARMAAPCSVFPCallConv = 68,
    LLVMMSP430INTRCallConv = 69,
    LLVMX86ThisCallCallConv = 70,
    LLVMPTXKernelCallConv = 71,
    LLVMPTXDeviceCallConv = 72,
    LLVMSPIRFUNCCallConv = 75,
    LLVMSPIRKERNELCallConv = 76,
    LLVMIntelOCLBICallConv = 77,
    LLVMX8664SysVCallConv = 78,
    LLVMWin64CallConv = 79,
    LLVMX86VectorCallCallConv = 80,
    LLVMHHVMCallConv = 81,
    LLVMHHVMCCallConv = 82,
    LLVMX86INTRCallConv = 83,
    LLVMAVRINTRCallConv = 84,
    LLVMAVRSIGNALCallConv = 85,
    LLVMAVRBUILTINCallConv = 86,
    LLVMAMDGPUVSCallConv = 87,
    LLVMAMDGPUGSCallConv = 88,
    LLVMAMDGPUPSCallConv = 89,
    LLVMAMDGPUCSCallConv = 90,
    LLVMAMDGPUKERNELCallConv = 91,
    LLVMX86RegCallCallConv = 92,
    LLVMAMDGPUHSCallConv = 93,
    LLVMMSP430BUILTINCallConv = 94,
    LLVMAMDGPULSCallConv = 95,
    LLVMAMDGPUESCallConv = 96,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L257 */
  export enum ValueKind {
    LLVMArgumentValueKind = 0,
    LLVMBasicBlockValueKind = 1,
    LLVMMemoryUseValueKind = 2,
    LLVMMemoryDefValueKind = 3,
    LLVMMemoryPhiValueKind = 4,
    LLVMFunctionValueKind = 5,
    LLVMGlobalAliasValueKind = 6,
    LLVMGlobalIFuncValueKind = 7,
    LLVMGlobalVariableValueKind = 8,
    LLVMBlockAddressValueKind = 9,
    LLVMConstantExprValueKind = 10,
    LLVMConstantArrayValueKind = 11,
    LLVMConstantStructValueKind = 12,
    LLVMConstantVectorValueKind = 13,
    LLVMUndefValueValueKind = 14,
    LLVMConstantAggregateZeroValueKind = 15,
    LLVMConstantDataArrayValueKind = 16,
    LLVMConstantDataVectorValueKind = 17,
    LLVMConstantIntValueKind = 18,
    LLVMConstantFPValueKind = 19,
    LLVMConstantPointerNullValueKind = 20,
    LLVMConstantTokenNoneValueKind = 21,
    LLVMMetadataAsValueValueKind = 22,
    LLVMInlineAsmValueKind = 23,
    LLVMInstructionValueKind = 24,
    LLVMPoisonValueValueKind = 25,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L290 */
  export enum IntPredicate {
    LLVMIntEQ = 32,
    LLVMIntNE = 33,
    LLVMIntUGT = 34,
    LLVMIntUGE = 35,
    LLVMIntULT = 36,
    LLVMIntULE = 37,
    LLVMIntSGT = 38,
    LLVMIntSGE = 39,
    LLVMIntSLT = 40,
    LLVMIntSLE = 41,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L303 */
  export enum RealPredicate {
    LLVMRealPredicateFalse = 0,
    LLVMRealOEQ = 1,
    LLVMRealOGT = 2,
    LLVMRealOGE = 3,
    LLVMRealOLT = 4,
    LLVMRealOLE = 5,
    LLVMRealONE = 6,
    LLVMRealORD = 7,
    LLVMRealUNO = 8,
    LLVMRealUEQ = 9,
    LLVMRealUGT = 10,
    LLVMRealUGE = 11,
    LLVMRealULT = 12,
    LLVMRealULE = 13,
    LLVMRealUNE = 14,
    LLVMRealPredicateTrue = 15,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L322 */
  export enum LandingPadClauseTy {
    LLVMLandingPadCatch = 0,
    LLVMLandingPadFilter = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L327 */
  export enum ThreadLocalMode {
    LLVMNotThreadLocal = 0,
    LLVMGeneralDynamicTLSModel = 1,
    LLVMLocalDynamicTLSModel = 2,
    LLVMInitialExecTLSModel = 3,
    LLVMLocalExecTLSModel = 4,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L335 */
  export enum AtomicOrdering {
    LLVMAtomicOrderingNotAtomic = 0,
    LLVMAtomicOrderingUnordered = 1,
    LLVMAtomicOrderingMonotonic = 2,
    LLVMAtomicOrderingAcquire = 4,
    LLVMAtomicOrderingRelease = 5,
    LLVMAtomicOrderingAcquireRelease = 6,
    LLVMAtomicOrderingSequentiallyConsistent = 7,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L362 */
  export enum AtomicRMWBinOp {
    LLVMAtomicRMWBinOpXchg = 0,
    LLVMAtomicRMWBinOpAdd = 1,
    LLVMAtomicRMWBinOpSub = 2,
    LLVMAtomicRMWBinOpAnd = 3,
    LLVMAtomicRMWBinOpNand = 4,
    LLVMAtomicRMWBinOpOr = 5,
    LLVMAtomicRMWBinOpXor = 6,
    LLVMAtomicRMWBinOpMax = 7,
    LLVMAtomicRMWBinOpMin = 8,
    LLVMAtomicRMWBinOpUMax = 9,
    LLVMAtomicRMWBinOpUMin = 10,
    LLVMAtomicRMWBinOpFAdd = 11,
    LLVMAtomicRMWBinOpFSub = 12,
    LLVMAtomicRMWBinOpFMax = 13,
    LLVMAtomicRMWBinOpFMin = 14,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L394 */
  export enum DiagnosticSeverity {
    LLVMDSError = 0,
    LLVMDSWarning = 1,
    LLVMDSRemark = 2,
    LLVMDSNote = 3,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L401 */
  export enum InlineAsmDialect {
    LLVMInlineAsmDialectATT = 0,
    LLVMInlineAsmDialectIntel = 1,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L406 */
  export enum ModuleFlagBehavior {
    LLVMModuleFlagBehaviorError = 0,
    LLVMModuleFlagBehaviorWarning = 1,
    LLVMModuleFlagBehaviorRequire = 2,
    LLVMModuleFlagBehaviorOverride = 3,
    LLVMModuleFlagBehaviorAppend = 4,
    LLVMModuleFlagBehaviorAppendUnique = 5,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L40 */
  export enum BinaryType {
    LLVMBinaryTypeArchive = 0,
    LLVMBinaryTypeMachOUniversalBinary = 1,
    LLVMBinaryTypeCOFFImportFile = 2,
    LLVMBinaryTypeIR = 3,
    LLVMBinaryTypeWinRes = 4,
    LLVMBinaryTypeCOFF = 5,
    LLVMBinaryTypeELF32L = 6,
    LLVMBinaryTypeELF32B = 7,
    LLVMBinaryTypeELF64L = 8,
    LLVMBinaryTypeELF64B = 9,
    LLVMBinaryTypeMachO32L = 10,
    LLVMBinaryTypeMachO32B = 11,
    LLVMBinaryTypeMachO64L = 12,
    LLVMBinaryTypeMachO64B = 13,
    LLVMBinaryTypeWasm = 14,
    LLVMBinaryTypeOffload = 15,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Comdat.h#L29 */
  export enum ComdatSelectionKind {
    LLVMAnyComdatSelectionKind = 0,
    LLVMExactMatchComdatSelectionKind = 1,
    LLVMLargestComdatSelectionKind = 2,
    LLVMNoDeduplicateComdatSelectionKind = 3,
    LLVMSameSizeComdatSelectionKind = 4,
  }

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Analysis.h#L44 */
  export declare function VerifyModule(M: LLVM.ModuleRef, Action: LLVM.VerifierFailureAction, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Analysis.h#L49 */
  export declare function VerifyFunction(Fn: LLVM.ValueRef, Action: LLVM.VerifierFailureAction): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Analysis.h#L53 */
  export declare function ViewFunctionCFG(Fn: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Analysis.h#L54 */
  export declare function ViewFunctionCFGOnly(Fn: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Targets.def#L41 */
  export declare function InitializeWebAssemblyTargetInfo(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Targets.def#L42 */
  export declare function InitializeX86TargetInfo(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Targets.def#L41 */
  export declare function InitializeWebAssemblyTarget(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Targets.def#L42 */
  export declare function InitializeX86Target(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Targets.def#L41 */
  export declare function InitializeWebAssemblyTargetMC(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Targets.def#L42 */
  export declare function InitializeX86TargetMC(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/AsmPrinters.def#L42 */
  export declare function InitializeWebAssemblyAsmPrinter(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/AsmPrinters.def#L43 */
  export declare function InitializeX86AsmPrinter(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/AsmParsers.def#L41 */
  export declare function InitializeWebAssemblyAsmParser(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/AsmParsers.def#L42 */
  export declare function InitializeX86AsmParser(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Disassemblers.def#L41 */
  export declare function InitializeWebAssemblyDisassembler(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm/Config/Disassemblers.def#L42 */
  export declare function InitializeX86Disassembler(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L186 */
  export declare function GetModuleDataLayout(M: LLVM.ModuleRef): LLVM.TargetDataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L193 */
  export declare function SetModuleDataLayout(M: LLVM.ModuleRef, DL: LLVM.TargetDataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L197 */
  export declare function CreateTargetData(StringRep: Pointer<"StringRep">): LLVM.TargetDataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L201 */
  export declare function DisposeTargetData(TD: LLVM.TargetDataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L206 */
  export declare function AddTargetLibraryInfo(TLI: LLVM.TargetLibraryInfoRef, PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L212 */
  export declare function CopyStringRepOfTargetData(TD: LLVM.TargetDataRef): Pointer<"LLVMCopyStringRepOfTargetData">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L217 */
  export declare function ByteOrder(TD: LLVM.TargetDataRef): LLVM.ByteOrdering;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L221 */
  export declare function PointerSize(TD: LLVM.TargetDataRef): Opaque<number, "LLVMPointerSize">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L226 */
  export declare function PointerSizeForAS(TD: LLVM.TargetDataRef, AS: Opaque<number, "AS">): Opaque<number, "LLVMPointerSizeForAS">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L230 */
  export declare function IntPtrType(TD: LLVM.TargetDataRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L235 */
  export declare function IntPtrTypeForAS(TD: LLVM.TargetDataRef, AS: Opaque<number, "AS">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L239 */
  export declare function IntPtrTypeInContext(C: LLVM.ContextRef, TD: LLVM.TargetDataRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L244 */
  export declare function IntPtrTypeForASInContext(C: LLVM.ContextRef, TD: LLVM.TargetDataRef, AS: Opaque<number, "AS">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L249 */
  export declare function SizeOfTypeInBits(TD: LLVM.TargetDataRef, Ty: LLVM.TypeRef): Opaque<bigint, "LLVMSizeOfTypeInBits">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L253 */
  export declare function StoreSizeOfType(TD: LLVM.TargetDataRef, Ty: LLVM.TypeRef): Opaque<bigint, "LLVMStoreSizeOfType">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L257 */
  export declare function ABISizeOfType(TD: LLVM.TargetDataRef, Ty: LLVM.TypeRef): Opaque<bigint, "LLVMABISizeOfType">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L261 */
  export declare function ABIAlignmentOfType(TD: LLVM.TargetDataRef, Ty: LLVM.TypeRef): Opaque<number, "LLVMABIAlignmentOfType">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L265 */
  export declare function CallFrameAlignmentOfType(TD: LLVM.TargetDataRef, Ty: LLVM.TypeRef): Opaque<number, "LLVMCallFrameAlignmentOfType">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L269 */
  export declare function PreferredAlignmentOfType(TD: LLVM.TargetDataRef, Ty: LLVM.TypeRef): Opaque<number, "LLVMPreferredAlignmentOfType">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L273 */
  export declare function PreferredAlignmentOfGlobal(TD: LLVM.TargetDataRef, GlobalVar: LLVM.ValueRef): Opaque<number, "LLVMPreferredAlignmentOfGlobal">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L278 */
  export declare function ElementAtOffset(TD: LLVM.TargetDataRef, StructTy: LLVM.TypeRef, Offset: Opaque<bigint, "Offset">): Opaque<number, "LLVMElementAtOffset">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Target.h#L283 */
  export declare function OffsetOfElement(TD: LLVM.TargetDataRef, StructTy: LLVM.TypeRef, Element: Opaque<number, "Element">): Opaque<bigint, "LLVMOffsetOfElement">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L70 */
  export declare function GetFirstTarget(): LLVM.TargetRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L72 */
  export declare function GetNextTarget(T: LLVM.TargetRef): LLVM.TargetRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L77 */
  export declare function GetTargetFromName(Name: Pointer<"Name">): LLVM.TargetRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L82 */
  export declare function GetTargetFromTriple(Triple: Pointer<"Triple">, T: Pointer<"T">, ErrorMessage: Pointer<"ErrorMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L86 */
  export declare function GetTargetName(T: LLVM.TargetRef): Pointer<"LLVMGetTargetName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L89 */
  export declare function GetTargetDescription(T: LLVM.TargetRef): Pointer<"LLVMGetTargetDescription">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L92 */
  export declare function TargetHasJIT(T: LLVM.TargetRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L95 */
  export declare function TargetHasTargetMachine(T: LLVM.TargetRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L98 */
  export declare function TargetHasAsmBackend(T: LLVM.TargetRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L102 */
  export declare function CreateTargetMachine(T: LLVM.TargetRef, Triple: Pointer<"Triple">, CPU: Pointer<"CPU">, Features: Pointer<"Features">, Level: LLVM.CodeGenOptLevel, Reloc: LLVM.RelocMode, CodeModel: LLVM.CodeModel): LLVM.TargetMachineRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L108 */
  export declare function DisposeTargetMachine(T: LLVM.TargetMachineRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L111 */
  export declare function GetTargetMachineTarget(T: LLVM.TargetMachineRef): LLVM.TargetRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L116 */
  export declare function GetTargetMachineTriple(T: LLVM.TargetMachineRef): Pointer<"LLVMGetTargetMachineTriple">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L121 */
  export declare function GetTargetMachineCPU(T: LLVM.TargetMachineRef): Pointer<"LLVMGetTargetMachineCPU">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L126 */
  export declare function GetTargetMachineFeatureString(T: LLVM.TargetMachineRef): Pointer<"LLVMGetTargetMachineFeatureString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L129 */
  export declare function CreateTargetDataLayout(T: LLVM.TargetMachineRef): LLVM.TargetDataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L132 */
  export declare function SetTargetMachineAsmVerbosity(T: LLVM.TargetMachineRef, VerboseAsm: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L138 */
  export declare function TargetMachineEmitToFile(T: LLVM.TargetMachineRef, M: LLVM.ModuleRef, Filename: Pointer<"Filename">, codegen: LLVM.CodeGenFileType, ErrorMessage: Pointer<"ErrorMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L142 */
  export declare function TargetMachineEmitToMemoryBuffer(T: LLVM.TargetMachineRef, M: LLVM.ModuleRef, codegen: LLVM.CodeGenFileType, ErrorMessage: Pointer<"ErrorMessage">, OutMemBuf: Pointer<"OutMemBuf">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L148 */
  export declare function GetDefaultTargetTriple(): Pointer<"LLVMGetDefaultTargetTriple">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L152 */
  export declare function NormalizeTargetTriple(triple: Pointer<"triple">): Pointer<"LLVMNormalizeTargetTriple">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L156 */
  export declare function GetHostCPUName(): Pointer<"LLVMGetHostCPUName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L160 */
  export declare function GetHostCPUFeatures(): Pointer<"LLVMGetHostCPUFeatures">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/TargetMachine.h#L163 */
  export declare function AddAnalysisPasses(T: LLVM.TargetMachineRef, PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L36 */
  export declare function LinkInMCJIT(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L37 */
  export declare function LinkInInterpreter(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L53 */
  export declare function CreateGenericValueOfInt(Ty: LLVM.TypeRef, N: Opaque<bigint, "N">, IsSigned: LLVM.Bool): LLVM.GenericValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L57 */
  export declare function CreateGenericValueOfPointer(P: Pointer<"P">): LLVM.GenericValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L59 */
  export declare function CreateGenericValueOfFloat(Ty: LLVM.TypeRef, N: Opaque<bigint, "N">): LLVM.GenericValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L61 */
  export declare function GenericValueIntWidth(GenValRef: LLVM.GenericValueRef): Opaque<number, "LLVMGenericValueIntWidth">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L63 */
  export declare function GenericValueToInt(GenVal: LLVM.GenericValueRef, IsSigned: LLVM.Bool): Opaque<bigint, "LLVMGenericValueToInt">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L66 */
  export declare function GenericValueToPointer(GenVal: LLVM.GenericValueRef): Pointer<"LLVMGenericValueToPointer">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L68 */
  export declare function GenericValueToFloat(TyRef: LLVM.TypeRef, GenVal: LLVM.GenericValueRef): Opaque<bigint, "LLVMGenericValueToFloat">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L70 */
  export declare function DisposeGenericValue(GenVal: LLVM.GenericValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L74 */
  export declare function CreateExecutionEngineForModule(OutEE: Pointer<"OutEE">, M: LLVM.ModuleRef, OutError: Pointer<"OutError">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L78 */
  export declare function CreateInterpreterForModule(OutInterp: Pointer<"OutInterp">, M: LLVM.ModuleRef, OutError: Pointer<"OutError">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L82 */
  export declare function CreateJITCompilerForModule(OutJIT: Pointer<"OutJIT">, M: LLVM.ModuleRef, OptLevel: Opaque<number, "OptLevel">, OutError: Pointer<"OutError">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L87 */
  export declare function InitializeMCJITCompilerOptions(Options: Pointer<"Options">, SizeOfOptions: Opaque<number, "SizeOfOptions">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L107 */
  export declare function CreateMCJITCompilerForModule(OutJIT: Pointer<"OutJIT">, M: LLVM.ModuleRef, Options: Pointer<"Options">, SizeOfOptions: Opaque<number, "SizeOfOptions">, OutError: Pointer<"OutError">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L112 */
  export declare function DisposeExecutionEngine(EE: LLVM.ExecutionEngineRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L114 */
  export declare function RunStaticConstructors(EE: LLVM.ExecutionEngineRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L116 */
  export declare function RunStaticDestructors(EE: LLVM.ExecutionEngineRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L118 */
  export declare function RunFunctionAsMain(EE: LLVM.ExecutionEngineRef, F: LLVM.ValueRef, ArgC: Opaque<number, "ArgC">, ArgV: Pointer<"ArgV">, EnvP: Pointer<"EnvP">): Opaque<number, "LLVMRunFunctionAsMain">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L122 */
  export declare function RunFunction(EE: LLVM.ExecutionEngineRef, F: LLVM.ValueRef, NumArgs: Opaque<number, "NumArgs">, Args: Pointer<"Args">): LLVM.GenericValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L126 */
  export declare function FreeMachineCodeForFunction(EE: LLVM.ExecutionEngineRef, F: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L128 */
  export declare function AddModule(EE: LLVM.ExecutionEngineRef, M: LLVM.ModuleRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L130 */
  export declare function RemoveModule(EE: LLVM.ExecutionEngineRef, M: LLVM.ModuleRef, OutMod: Pointer<"OutMod">, OutError: Pointer<"OutError">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L133 */
  export declare function FindFunction(EE: LLVM.ExecutionEngineRef, Name: Pointer<"Name">, OutFn: Pointer<"OutFn">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L136 */
  export declare function RecompileAndRelinkFunction(EE: LLVM.ExecutionEngineRef, Fn: LLVM.ValueRef): Pointer<"LLVMRecompileAndRelinkFunction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L139 */
  export declare function GetExecutionEngineTargetData(EE: LLVM.ExecutionEngineRef): LLVM.TargetDataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L141 */
  export declare function GetExecutionEngineTargetMachine(EE: LLVM.ExecutionEngineRef): LLVM.TargetMachineRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L143 */
  export declare function AddGlobalMapping(EE: LLVM.ExecutionEngineRef, Global: LLVM.ValueRef, Addr: Pointer<"Addr">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L146 */
  export declare function GetPointerToGlobal(EE: LLVM.ExecutionEngineRef, Global: LLVM.ValueRef): Pointer<"LLVMGetPointerToGlobal">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L148 */
  export declare function GetGlobalValueAddress(EE: LLVM.ExecutionEngineRef, Name: Pointer<"Name">): Opaque<bigint, "LLVMGetGlobalValueAddress">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L150 */
  export declare function GetFunctionAddress(EE: LLVM.ExecutionEngineRef, Name: Pointer<"Name">): Opaque<bigint, "LLVMGetFunctionAddress">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L154 */
  export declare function ExecutionEngineGetErrMsg(EE: LLVM.ExecutionEngineRef, OutError: Pointer<"OutError">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L180 */
  export declare function CreateSimpleMCJITMemoryManager(Opaque: Pointer<"Opaque">, AllocateCodeSection: LLVM.MemoryManagerAllocateCodeSectionCallback, AllocateDataSection: LLVM.MemoryManagerAllocateDataSectionCallback, FinalizeMemory: LLVM.MemoryManagerFinalizeMemoryCallback, Destroy: LLVM.MemoryManagerDestroyCallback): LLVM.MCJITMemoryManagerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L187 */
  export declare function DisposeMCJITMemoryManager(MM: LLVM.MCJITMemoryManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L191 */
  export declare function CreateGDBRegistrationListener(): LLVM.JITEventListenerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L192 */
  export declare function CreateIntelJITEventListener(): LLVM.JITEventListenerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L193 */
  export declare function CreateOProfileJITEventListener(): LLVM.JITEventListenerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ExecutionEngine.h#L194 */
  export declare function CreatePerfJITEventListener(): LLVM.JITEventListenerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Disassembler.h#L38 */
  export declare function CreateDisasm(TripleName: Pointer<"TripleName">, DisInfo: Pointer<"DisInfo">, TagType: Opaque<number, "TagType">, GetOpInfo: LLVM.OpInfoCallback, SymbolLookUp: LLVM.SymbolLookupCallback): LLVM.DisasmContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Disassembler.h#L50 */
  export declare function CreateDisasmCPU(Triple: Pointer<"Triple">, CPU: Pointer<"CPU">, DisInfo: Pointer<"DisInfo">, TagType: Opaque<number, "TagType">, GetOpInfo: LLVM.OpInfoCallback, SymbolLookUp: LLVM.SymbolLookupCallback): LLVM.DisasmContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Disassembler.h#L63 */
  export declare function CreateDisasmCPUFeatures(Triple: Pointer<"Triple">, CPU: Pointer<"CPU">, Features: Pointer<"Features">, DisInfo: Pointer<"DisInfo">, TagType: Opaque<number, "TagType">, GetOpInfo: LLVM.OpInfoCallback, SymbolLookUp: LLVM.SymbolLookupCallback): LLVM.DisasmContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Disassembler.h#L72 */
  export declare function SetDisasmOptions(DC: LLVM.DisasmContextRef, Options: Opaque<bigint, "Options">): Opaque<number, "LLVMSetDisasmOptions">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Disassembler.h#L88 */
  export declare function DisasmDispose(DC: LLVM.DisasmContextRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Disassembler.h#L100 */
  export declare function DisasmInstruction(DC: LLVM.DisasmContextRef, Bytes: Pointer<"Bytes">, BytesSize: Opaque<bigint, "BytesSize">, PC: Opaque<bigint, "PC">, OutString: Pointer<"OutString">, OutStringSize: Opaque<number, "OutStringSize">): Opaque<number, "LLVMDisasmInstruction">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L197 */
  export declare function DebugMetadataVersion(): Opaque<number, "LLVMDebugMetadataVersion">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L202 */
  export declare function GetModuleDebugMetadataVersion(Module: LLVM.ModuleRef): Opaque<number, "LLVMGetModuleDebugMetadataVersion">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L210 */
  export declare function StripModuleDebugInfo(Module: LLVM.ModuleRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L216 */
  export declare function CreateDIBuilderDisallowUnresolved(M: LLVM.ModuleRef): LLVM.DIBuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L223 */
  export declare function CreateDIBuilder(M: LLVM.ModuleRef): LLVM.DIBuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L229 */
  export declare function DisposeDIBuilder(Builder: LLVM.DIBuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L234 */
  export declare function DIBuilderFinalize(Builder: LLVM.DIBuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L240 */
  export declare function DIBuilderFinalizeSubprogram(Builder: LLVM.DIBuilderRef, Subprogram: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L275 */
  export declare function DIBuilderCreateCompileUnit(Builder: LLVM.DIBuilderRef, Lang: LLVM.DWARFSourceLanguage, FileRef: LLVM.MetadataRef, Producer: Pointer<"Producer">, ProducerLen: Opaque<number, "ProducerLen">, isOptimized: LLVM.Bool, Flags: Pointer<"Flags">, FlagsLen: Opaque<number, "FlagsLen">, RuntimeVer: Opaque<number, "RuntimeVer">, SplitName: Pointer<"SplitName">, SplitNameLen: Opaque<number, "SplitNameLen">, Kind: LLVM.DWARFEmissionKind, DWOId: Opaque<number, "DWOId">, SplitDebugInlining: LLVM.Bool, DebugInfoForProfiling: LLVM.Bool, SysRoot: Pointer<"SysRoot">, SysRootLen: Opaque<number, "SysRootLen">, SDK: Pointer<"SDK">, SDKLen: Opaque<number, "SDKLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L293 */
  export declare function DIBuilderCreateFile(Builder: LLVM.DIBuilderRef, Filename: Pointer<"Filename">, FilenameLen: Opaque<number, "FilenameLen">, Directory: Pointer<"Directory">, DirectoryLen: Opaque<number, "DirectoryLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L312 */
  export declare function DIBuilderCreateModule(Builder: LLVM.DIBuilderRef, ParentScope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, ConfigMacros: Pointer<"ConfigMacros">, ConfigMacrosLen: Opaque<number, "ConfigMacrosLen">, IncludePath: Pointer<"IncludePath">, IncludePathLen: Opaque<number, "IncludePathLen">, APINotesFile: Pointer<"APINotesFile">, APINotesFileLen: Opaque<number, "APINotesFileLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L328 */
  export declare function DIBuilderCreateNameSpace(Builder: LLVM.DIBuilderRef, ParentScope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, ExportSymbols: LLVM.Bool): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L351 */
  export declare function DIBuilderCreateFunction(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, LinkageName: Pointer<"LinkageName">, LinkageNameLen: Opaque<number, "LinkageNameLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, Ty: LLVM.MetadataRef, IsLocalToUnit: LLVM.Bool, IsDefinition: LLVM.Bool, ScopeLine: Opaque<number, "ScopeLine">, Flags: LLVM.DIFlags, IsOptimized: LLVM.Bool): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L366 */
  export declare function DIBuilderCreateLexicalBlock(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">, Column: Opaque<number, "Column">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L378 */
  export declare function DIBuilderCreateLexicalBlockFile(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, File: LLVM.MetadataRef, Discriminator: Opaque<number, "Discriminator">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L392 */
  export declare function DIBuilderCreateImportedModuleFromNamespace(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, NS: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L409 */
  export declare function DIBuilderCreateImportedModuleFromAlias(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, ImportedEntity: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L424 */
  export declare function DIBuilderCreateImportedModuleFromModule(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, M: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L444 */
  export declare function DIBuilderCreateImportedDeclaration(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Decl: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L460 */
  export declare function DIBuilderCreateDebugLocation(Ctx: LLVM.ContextRef, Line: Opaque<number, "Line">, Column: Opaque<number, "Column">, Scope: LLVM.MetadataRef, InlinedAt: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L470 */
  export declare function DILocationGetLine(Location: LLVM.MetadataRef): Opaque<number, "LLVMDILocationGetLine">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L478 */
  export declare function DILocationGetColumn(Location: LLVM.MetadataRef): Opaque<number, "LLVMDILocationGetColumn">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L486 */
  export declare function DILocationGetScope(Location: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L494 */
  export declare function DILocationGetInlinedAt(Location: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L502 */
  export declare function DIScopeGetFile(Scope: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L511 */
  export declare function DIFileGetDirectory(File: LLVM.MetadataRef, Len: Pointer<"Len">): Pointer<"LLVMDIFileGetDirectory">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L520 */
  export declare function DIFileGetFilename(File: LLVM.MetadataRef, Len: Pointer<"Len">): Pointer<"LLVMDIFileGetFilename">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L529 */
  export declare function DIFileGetSource(File: LLVM.MetadataRef, Len: Pointer<"Len">): Pointer<"LLVMDIFileGetSource">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L537 */
  export declare function DIBuilderGetOrCreateTypeArray(Builder: LLVM.DIBuilderRef, Data: Pointer<"Data">, NumElements: Opaque<number, "NumElements">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L552 */
  export declare function DIBuilderCreateSubroutineType(Builder: LLVM.DIBuilderRef, File: LLVM.MetadataRef, ParameterTypes: Pointer<"ParameterTypes">, NumParameterTypes: Opaque<number, "NumParameterTypes">, Flags: LLVM.DIFlags): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L569 */
  export declare function DIBuilderCreateMacro(Builder: LLVM.DIBuilderRef, ParentMacroFile: LLVM.MetadataRef, Line: Opaque<number, "Line">, RecordType: LLVM.DWARFMacinfoRecordType, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Value: Pointer<"Value">, ValueLen: Opaque<number, "ValueLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L586 */
  export declare function DIBuilderCreateTempMacroFile(Builder: LLVM.DIBuilderRef, ParentMacroFile: LLVM.MetadataRef, Line: Opaque<number, "Line">, File: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L598 */
  export declare function DIBuilderCreateEnumerator(Builder: LLVM.DIBuilderRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Value: Opaque<bigint, "Value">, IsUnsigned: LLVM.Bool): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L617 */
  export declare function DIBuilderCreateEnumerationType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNumber: Opaque<number, "LineNumber">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">, ClassTy: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L640 */
  export declare function DIBuilderCreateUnionType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNumber: Opaque<number, "LineNumber">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, Flags: LLVM.DIFlags, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">, RunTimeLang: Opaque<number, "RunTimeLang">, UniqueId: Pointer<"UniqueId">, UniqueIdLen: Opaque<number, "UniqueIdLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L658 */
  export declare function DIBuilderCreateArrayType(Builder: LLVM.DIBuilderRef, Size: Opaque<bigint, "Size">, AlignInBits: Opaque<number, "AlignInBits">, Ty: LLVM.MetadataRef, Subscripts: Pointer<"Subscripts">, NumSubscripts: Opaque<number, "NumSubscripts">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L673 */
  export declare function DIBuilderCreateVectorType(Builder: LLVM.DIBuilderRef, Size: Opaque<bigint, "Size">, AlignInBits: Opaque<number, "AlignInBits">, Ty: LLVM.MetadataRef, Subscripts: Pointer<"Subscripts">, NumSubscripts: Opaque<number, "NumSubscripts">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L685 */
  export declare function DIBuilderCreateUnspecifiedType(Builder: LLVM.DIBuilderRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L699 */
  export declare function DIBuilderCreateBasicType(Builder: LLVM.DIBuilderRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, SizeInBits: Opaque<bigint, "SizeInBits">, Encoding: LLVM.DWARFTypeEncoding, Flags: LLVM.DIFlags): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L714 */
  export declare function DIBuilderCreatePointerType(Builder: LLVM.DIBuilderRef, PointeeTy: LLVM.MetadataRef, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, AddressSpace: Opaque<number, "AddressSpace">, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L737 */
  export declare function DIBuilderCreateStructType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNumber: Opaque<number, "LineNumber">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, Flags: LLVM.DIFlags, DerivedFrom: LLVM.MetadataRef, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">, RunTimeLang: Opaque<number, "RunTimeLang">, VTableHolder: LLVM.MetadataRef, UniqueId: Pointer<"UniqueId">, UniqueIdLen: Opaque<number, "UniqueIdLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L759 */
  export declare function DIBuilderCreateMemberType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, OffsetInBits: Opaque<bigint, "OffsetInBits">, Flags: LLVM.DIFlags, Ty: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L780 */
  export declare function DIBuilderCreateStaticMemberType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNumber: Opaque<number, "LineNumber">, Type: LLVM.MetadataRef, Flags: LLVM.DIFlags, ConstantVal: LLVM.ValueRef, AlignInBits: Opaque<number, "AlignInBits">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L796 */
  export declare function DIBuilderCreateMemberPointerType(Builder: LLVM.DIBuilderRef, PointeeType: LLVM.MetadataRef, ClassType: LLVM.MetadataRef, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, Flags: LLVM.DIFlags): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L817 */
  export declare function DIBuilderCreateObjCIVar(Builder: LLVM.DIBuilderRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, OffsetInBits: Opaque<bigint, "OffsetInBits">, Flags: LLVM.DIFlags, Ty: LLVM.MetadataRef, PropertyNode: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L839 */
  export declare function DIBuilderCreateObjCProperty(Builder: LLVM.DIBuilderRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, GetterName: Pointer<"GetterName">, GetterNameLen: Opaque<number, "GetterNameLen">, SetterName: Pointer<"SetterName">, SetterNameLen: Opaque<number, "SetterNameLen">, PropertyAttributes: Opaque<number, "PropertyAttributes">, Ty: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L853 */
  export declare function DIBuilderCreateObjectPointerType(Builder: LLVM.DIBuilderRef, Type: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L865 */
  export declare function DIBuilderCreateQualifiedType(Builder: LLVM.DIBuilderRef, Tag: Opaque<number, "Tag">, Type: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L876 */
  export declare function DIBuilderCreateReferenceType(Builder: LLVM.DIBuilderRef, Tag: Opaque<number, "Tag">, Type: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L884 */
  export declare function DIBuilderCreateNullPtrType(Builder: LLVM.DIBuilderRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L896 */
  export declare function DIBuilderCreateTypedef(Builder: LLVM.DIBuilderRef, Type: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, Scope: LLVM.MetadataRef, AlignInBits: Opaque<number, "AlignInBits">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L912 */
  export declare function DIBuilderCreateInheritance(Builder: LLVM.DIBuilderRef, Ty: LLVM.MetadataRef, BaseTy: LLVM.MetadataRef, BaseOffset: Opaque<bigint, "BaseOffset">, VBPtrOffset: Opaque<number, "VBPtrOffset">, Flags: LLVM.DIFlags): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L933 */
  export declare function DIBuilderCreateForwardDecl(Builder: LLVM.DIBuilderRef, Tag: Opaque<number, "Tag">, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Scope: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">, RuntimeLang: Opaque<number, "RuntimeLang">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, UniqueIdentifier: Pointer<"UniqueIdentifier">, UniqueIdentifierLen: Opaque<number, "UniqueIdentifierLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L957 */
  export declare function DIBuilderCreateReplaceableCompositeType(Builder: LLVM.DIBuilderRef, Tag: Opaque<number, "Tag">, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Scope: LLVM.MetadataRef, File: LLVM.MetadataRef, Line: Opaque<number, "Line">, RuntimeLang: Opaque<number, "RuntimeLang">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, Flags: LLVM.DIFlags, UniqueIdentifier: Pointer<"UniqueIdentifier">, UniqueIdentifierLen: Opaque<number, "UniqueIdentifierLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L979 */
  export declare function DIBuilderCreateBitFieldMemberType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNumber: Opaque<number, "LineNumber">, SizeInBits: Opaque<bigint, "SizeInBits">, OffsetInBits: Opaque<bigint, "OffsetInBits">, StorageOffsetInBits: Opaque<bigint, "StorageOffsetInBits">, Flags: LLVM.DIFlags, Type: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1010 */
  export declare function DIBuilderCreateClassType(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNumber: Opaque<number, "LineNumber">, SizeInBits: Opaque<bigint, "SizeInBits">, AlignInBits: Opaque<number, "AlignInBits">, OffsetInBits: Opaque<bigint, "OffsetInBits">, Flags: LLVM.DIFlags, DerivedFrom: LLVM.MetadataRef, Elements: Pointer<"Elements">, NumElements: Opaque<number, "NumElements">, VTableHolder: LLVM.MetadataRef, TemplateParamsNode: LLVM.MetadataRef, UniqueIdentifier: Pointer<"UniqueIdentifier">, UniqueIdentifierLen: Opaque<number, "UniqueIdentifierLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1025 */
  export declare function DIBuilderCreateArtificialType(Builder: LLVM.DIBuilderRef, Type: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1035 */
  export declare function DITypeGetName(DType: LLVM.MetadataRef, Length: Pointer<"Length">): Pointer<"LLVMDITypeGetName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1043 */
  export declare function DITypeGetSizeInBits(DType: LLVM.MetadataRef): Opaque<bigint, "LLVMDITypeGetSizeInBits">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1051 */
  export declare function DITypeGetOffsetInBits(DType: LLVM.MetadataRef): Opaque<bigint, "LLVMDITypeGetOffsetInBits">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1059 */
  export declare function DITypeGetAlignInBits(DType: LLVM.MetadataRef): Opaque<number, "LLVMDITypeGetAlignInBits">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1067 */
  export declare function DITypeGetLine(DType: LLVM.MetadataRef): Opaque<number, "LLVMDITypeGetLine">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1075 */
  export declare function DITypeGetFlags(DType: LLVM.MetadataRef): LLVM.DIFlags;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1083 */
  export declare function DIBuilderGetOrCreateSubrange(Builder: LLVM.DIBuilderRef, LowerBound: Opaque<bigint, "LowerBound">, Count: Opaque<bigint, "Count">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1093 */
  export declare function DIBuilderGetOrCreateArray(Builder: LLVM.DIBuilderRef, Data: Pointer<"Data">, NumElements: Opaque<number, "NumElements">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1104 */
  export declare function DIBuilderCreateExpression(Builder: LLVM.DIBuilderRef, Addr: Pointer<"Addr">, Length: Opaque<number, "Length">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1114 */
  export declare function DIBuilderCreateConstantValueExpression(Builder: LLVM.DIBuilderRef, Value: Opaque<bigint, "Value">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1136 */
  export declare function DIBuilderCreateGlobalVariableExpression(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Linkage: Pointer<"Linkage">, LinkLen: Opaque<number, "LinkLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, Ty: LLVM.MetadataRef, LocalToUnit: LLVM.Bool, Expr: LLVM.MetadataRef, Decl: LLVM.MetadataRef, AlignInBits: Opaque<number, "AlignInBits">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1148 */
  export declare function DIGlobalVariableExpressionGetVariable(GVE: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1156 */
  export declare function DIGlobalVariableExpressionGetExpression(GVE: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1165 */
  export declare function DIVariableGetFile(Var: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1173 */
  export declare function DIVariableGetScope(Var: LLVM.MetadataRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1181 */
  export declare function DIVariableGetLine(Var: LLVM.MetadataRef): Opaque<number, "LLVMDIVariableGetLine">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1191 */
  export declare function TemporaryMDNode(Ctx: LLVM.ContextRef, Data: Pointer<"Data">, NumElements: Opaque<number, "NumElements">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1201 */
  export declare function DisposeTemporaryMDNode(TempNode: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1208 */
  export declare function MetadataReplaceAllUsesWith(TempTargetMetadata: LLVM.MetadataRef, Replacement: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1228 */
  export declare function DIBuilderCreateTempGlobalVariableFwdDecl(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Linkage: Pointer<"Linkage">, LnkLen: Opaque<number, "LnkLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, Ty: LLVM.MetadataRef, LocalToUnit: LLVM.Bool, Decl: LLVM.MetadataRef, AlignInBits: Opaque<number, "AlignInBits">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1243 */
  export declare function DIBuilderInsertDeclareBefore(Builder: LLVM.DIBuilderRef, Storage: LLVM.ValueRef, VarInfo: LLVM.MetadataRef, Expr: LLVM.MetadataRef, DebugLoc: LLVM.MetadataRef, Instr: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1258 */
  export declare function DIBuilderInsertDeclareAtEnd(Builder: LLVM.DIBuilderRef, Storage: LLVM.ValueRef, VarInfo: LLVM.MetadataRef, Expr: LLVM.MetadataRef, DebugLoc: LLVM.MetadataRef, Block: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1271 */
  export declare function DIBuilderInsertDbgValueBefore(Builder: LLVM.DIBuilderRef, Val: LLVM.ValueRef, VarInfo: LLVM.MetadataRef, Expr: LLVM.MetadataRef, DebugLoc: LLVM.MetadataRef, Instr: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1289 */
  export declare function DIBuilderInsertDbgValueAtEnd(Builder: LLVM.DIBuilderRef, Val: LLVM.ValueRef, VarInfo: LLVM.MetadataRef, Expr: LLVM.MetadataRef, DebugLoc: LLVM.MetadataRef, Block: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1309 */
  export declare function DIBuilderCreateAutoVariable(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, Ty: LLVM.MetadataRef, AlwaysPreserve: LLVM.Bool, Flags: LLVM.DIFlags, AlignInBits: Opaque<number, "AlignInBits">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1327 */
  export declare function DIBuilderCreateParameterVariable(Builder: LLVM.DIBuilderRef, Scope: LLVM.MetadataRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, ArgNo: Opaque<number, "ArgNo">, File: LLVM.MetadataRef, LineNo: Opaque<number, "LineNo">, Ty: LLVM.MetadataRef, AlwaysPreserve: LLVM.Bool, Flags: LLVM.DIFlags): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1337 */
  export declare function GetSubprogram(Func: LLVM.ValueRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1344 */
  export declare function SetSubprogram(Func: LLVM.ValueRef, SP: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1352 */
  export declare function DISubprogramGetLine(Subprogram: LLVM.MetadataRef): Opaque<number, "LLVMDISubprogramGetLine">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1359 */
  export declare function InstructionGetDebugLoc(Inst: LLVM.ValueRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1368 */
  export declare function InstructionSetDebugLoc(Inst: LLVM.ValueRef, Loc: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/DebugInfo.h#L1375 */
  export declare function GetMetadataKind(Metadata: LLVM.MetadataRef): LLVM.MetadataKind;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L44 */
  export declare function GetErrorTypeId(Err: LLVM.ErrorRef): LLVM.ErrorTypeId;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L52 */
  export declare function ConsumeError(Err: LLVM.ErrorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L60 */
  export declare function GetErrorMessage(Err: LLVM.ErrorRef): Pointer<"LLVMGetErrorMessage">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L65 */
  export declare function DisposeErrorMessage(ErrMsg: Pointer<"ErrMsg">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L70 */
  export declare function GetStringErrorTypeId(): LLVM.ErrorTypeId;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Error.h#L75 */
  export declare function CreateStringError(ErrMsg: Pointer<"ErrMsg">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L457 */
  export declare function OrcExecutionSessionSetErrorReporter(ES: LLVM.OrcExecutionSessionRef, ReportError: LLVM.OrcErrorReporterFunction, Ctx: Pointer<"Ctx">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L468 */
  export declare function OrcExecutionSessionGetSymbolStringPool(ES: LLVM.OrcExecutionSessionRef): LLVM.OrcSymbolStringPoolRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L480 */
  export declare function OrcSymbolStringPoolClearDeadEntries(SSP: LLVM.OrcSymbolStringPoolRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L495 */
  export declare function OrcExecutionSessionIntern(ES: LLVM.OrcExecutionSessionRef, Name: Pointer<"Name">): LLVM.OrcSymbolStringPoolEntryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L500 */
  export declare function OrcRetainSymbolStringPoolEntry(S: LLVM.OrcSymbolStringPoolEntryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L505 */
  export declare function OrcReleaseSymbolStringPoolEntry(S: LLVM.OrcSymbolStringPoolEntryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L507 */
  export declare function OrcSymbolStringPoolEntryStr(S: LLVM.OrcSymbolStringPoolEntryRef): Pointer<"LLVMOrcSymbolStringPoolEntryStr">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L512 */
  export declare function OrcReleaseResourceTracker(RT: LLVM.OrcResourceTrackerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L518 */
  export declare function OrcResourceTrackerTransferTo(SrcRT: LLVM.OrcResourceTrackerRef, DstRT: LLVM.OrcResourceTrackerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L525 */
  export declare function OrcResourceTrackerRemove(RT: LLVM.OrcResourceTrackerRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L532 */
  export declare function OrcDisposeDefinitionGenerator(DG: LLVM.OrcDefinitionGeneratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L537 */
  export declare function OrcDisposeMaterializationUnit(MU: LLVM.OrcMaterializationUnitRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L572 */
  export declare function OrcCreateCustomMaterializationUnit(Name: Pointer<"Name">, Ctx: Pointer<"Ctx">, Syms: LLVM.OrcCSymbolFlagsMapPairs, NumSyms: Opaque<number, "NumSyms">, InitSym: LLVM.OrcSymbolStringPoolEntryRef, Materialize: LLVM.OrcMaterializationUnitMaterializeFunction, Discard: LLVM.OrcMaterializationUnitDiscardFunction, Destroy: LLVM.OrcMaterializationUnitDestroyFunction): LLVM.OrcMaterializationUnitRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L601 */
  export declare function OrcAbsoluteSymbols(Syms: LLVM.OrcCSymbolMapPairs, NumPairs: Opaque<number, "NumPairs">): LLVM.OrcMaterializationUnitRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L624 */
  export declare function OrcLazyReexports(LCTM: LLVM.OrcLazyCallThroughManagerRef, ISM: LLVM.OrcIndirectStubsManagerRef, SourceRef: LLVM.OrcJITDylibRef, CallableAliases: LLVM.OrcCSymbolAliasMapPairs, NumPairs: Opaque<number, "NumPairs">): LLVM.OrcMaterializationUnitRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L639 */
  export declare function OrcDisposeMaterializationResponsibility(MR: LLVM.OrcMaterializationResponsibilityRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L645 */
  export declare function OrcMaterializationResponsibilityGetTargetDylib(MR: LLVM.OrcMaterializationResponsibilityRef): LLVM.OrcJITDylibRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L652 */
  export declare function OrcMaterializationResponsibilityGetExecutionSession(MR: LLVM.OrcMaterializationResponsibilityRef): LLVM.OrcExecutionSessionRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L665 */
  export declare function OrcMaterializationResponsibilityGetSymbols(MR: LLVM.OrcMaterializationResponsibilityRef, NumPairs: Pointer<"NumPairs">): LLVM.OrcCSymbolFlagsMapPairs;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L673 */
  export declare function OrcDisposeCSymbolFlagsMap(Pairs: LLVM.OrcCSymbolFlagsMapPairs): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L684 */
  export declare function OrcMaterializationResponsibilityGetInitializerSymbol(MR: LLVM.OrcMaterializationResponsibilityRef): LLVM.OrcSymbolStringPoolEntryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L694 */
  export declare function OrcMaterializationResponsibilityGetRequestedSymbols(MR: LLVM.OrcMaterializationResponsibilityRef, NumSymbols: Pointer<"NumSymbols">): Pointer<"LLVMOrcMaterializationResponsibilityGetRequestedSymbols">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L702 */
  export declare function OrcDisposeSymbols(Symbols: Pointer<"Symbols">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L720 */
  export declare function OrcMaterializationResponsibilityNotifyResolved(MR: LLVM.OrcMaterializationResponsibilityRef, Symbols: LLVM.OrcCSymbolMapPairs, NumPairs: Opaque<number, "NumPairs">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L737 */
  export declare function OrcMaterializationResponsibilityNotifyEmitted(MR: LLVM.OrcMaterializationResponsibilityRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L753 */
  export declare function OrcMaterializationResponsibilityDefineMaterializing(MR: LLVM.OrcMaterializationResponsibilityRef, Pairs: LLVM.OrcCSymbolFlagsMapPairs, NumPairs: Opaque<number, "NumPairs">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L764 */
  export declare function OrcMaterializationResponsibilityFailMaterialization(MR: LLVM.OrcMaterializationResponsibilityRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L774 */
  export declare function OrcMaterializationResponsibilityReplace(MR: LLVM.OrcMaterializationResponsibilityRef, MU: LLVM.OrcMaterializationUnitRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L786 */
  export declare function OrcMaterializationResponsibilityDelegate(MR: LLVM.OrcMaterializationResponsibilityRef, Symbols: Pointer<"Symbols">, NumSymbols: Opaque<number, "NumSymbols">, Result: Pointer<"Result">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L809 */
  export declare function OrcMaterializationResponsibilityAddDependencies(MR: LLVM.OrcMaterializationResponsibilityRef, Name: LLVM.OrcSymbolStringPoolEntryRef, Dependencies: LLVM.OrcCDependenceMapPairs, NumPairs: Opaque<number, "NumPairs">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L819 */
  export declare function OrcMaterializationResponsibilityAddDependenciesForAll(MR: LLVM.OrcMaterializationResponsibilityRef, Dependencies: LLVM.OrcCDependenceMapPairs, NumPairs: Opaque<number, "NumPairs">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L833 */
  export declare function OrcExecutionSessionCreateBareJITDylib(ES: LLVM.OrcExecutionSessionRef, Name: Pointer<"Name">): LLVM.OrcJITDylibRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L849 */
  export declare function OrcExecutionSessionCreateJITDylib(ES: LLVM.OrcExecutionSessionRef, Result: Pointer<"Result">, Name: Pointer<"Name">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L858 */
  export declare function OrcExecutionSessionGetJITDylibByName(ES: LLVM.OrcExecutionSessionRef, Name: Pointer<"Name">): LLVM.OrcJITDylibRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L867 */
  export declare function OrcJITDylibCreateResourceTracker(JD: LLVM.OrcJITDylibRef): LLVM.OrcResourceTrackerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L875 */
  export declare function OrcJITDylibGetDefaultResourceTracker(JD: LLVM.OrcJITDylibRef): LLVM.OrcResourceTrackerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L884 */
  export declare function OrcJITDylibDefine(JD: LLVM.OrcJITDylibRef, MU: LLVM.OrcMaterializationUnitRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L891 */
  export declare function OrcJITDylibClear(JD: LLVM.OrcJITDylibRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L899 */
  export declare function OrcJITDylibAddGenerator(JD: LLVM.OrcJITDylibRef, DG: LLVM.OrcDefinitionGeneratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L905 */
  export declare function OrcCreateCustomCAPIDefinitionGenerator(F: LLVM.OrcCAPIDefinitionGeneratorTryToGenerateFunction, Ctx: Pointer<"Ctx">): LLVM.OrcDefinitionGeneratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L926 */
  export declare function OrcCreateDynamicLibrarySearchGeneratorForProcess(Result: Pointer<"Result">, GlobalPrefx: Opaque<number, "GlobalPrefx">, Filter: LLVM.OrcSymbolPredicate, FilterCtx: Pointer<"FilterCtx">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L951 */
  export declare function OrcCreateDynamicLibrarySearchGeneratorForPath(Result: Pointer<"Result">, FileName: Pointer<"FileName">, GlobalPrefix: Opaque<number, "GlobalPrefix">, Filter: LLVM.OrcSymbolPredicate, FilterCtx: Pointer<"FilterCtx">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L969 */
  export declare function OrcCreateStaticLibrarySearchGeneratorForPath(Result: Pointer<"Result">, ObjLayer: LLVM.OrcObjectLayerRef, FileName: Pointer<"FileName">, TargetTriple: Pointer<"TargetTriple">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L981 */
  export declare function OrcCreateNewThreadSafeContext(): LLVM.OrcThreadSafeContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L987 */
  export declare function OrcThreadSafeContextGetContext(TSCtx: LLVM.OrcThreadSafeContextRef): LLVM.ContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L992 */
  export declare function OrcDisposeThreadSafeContext(TSCtx: LLVM.OrcThreadSafeContextRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1005 */
  export declare function OrcCreateNewThreadSafeModule(M: LLVM.ModuleRef, TSCtx: LLVM.OrcThreadSafeContextRef): LLVM.OrcThreadSafeModuleRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1013 */
  export declare function OrcDisposeThreadSafeModule(TSM: LLVM.OrcThreadSafeModuleRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1019 */
  export declare function OrcThreadSafeModuleWithModuleDo(TSM: LLVM.OrcThreadSafeModuleRef, F: LLVM.OrcGenericIRModuleOperationFunction, Ctx: Pointer<"Ctx">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1031 */
  export declare function OrcJITTargetMachineBuilderDetectHost(Result: Pointer<"Result">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1044 */
  export declare function OrcJITTargetMachineBuilderCreateFromTargetMachine(TM: LLVM.TargetMachineRef): LLVM.OrcJITTargetMachineBuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1049 */
  export declare function OrcDisposeJITTargetMachineBuilder(JTMB: LLVM.OrcJITTargetMachineBuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1058 */
  export declare function OrcJITTargetMachineBuilderGetTargetTriple(JTMB: LLVM.OrcJITTargetMachineBuilderRef): Pointer<"LLVMOrcJITTargetMachineBuilderGetTargetTriple">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1065 */
  export declare function OrcJITTargetMachineBuilderSetTargetTriple(JTMB: LLVM.OrcJITTargetMachineBuilderRef, TargetTriple: Pointer<"TargetTriple">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1079 */
  export declare function OrcObjectLayerAddObjectFile(ObjLayer: LLVM.OrcObjectLayerRef, JD: LLVM.OrcJITDylibRef, ObjBuffer: LLVM.MemoryBufferRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1105 */
  export declare function OrcObjectLayerEmit(ObjLayer: LLVM.OrcObjectLayerRef, R: LLVM.OrcMaterializationResponsibilityRef, ObjBuffer: LLVM.MemoryBufferRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1112 */
  export declare function OrcDisposeObjectLayer(ObjLayer: LLVM.OrcObjectLayerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1114 */
  export declare function OrcIRTransformLayerEmit(IRTransformLayer: LLVM.OrcIRTransformLayerRef, MR: LLVM.OrcMaterializationResponsibilityRef, TSM: LLVM.OrcThreadSafeModuleRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1122 */
  export declare function OrcIRTransformLayerSetTransform(IRTransformLayer: LLVM.OrcIRTransformLayerRef, TransformFunction: LLVM.OrcIRTransformLayerTransformFunction, Ctx: Pointer<"Ctx">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1129 */
  export declare function OrcObjectTransformLayerSetTransform(ObjTransformLayer: LLVM.OrcObjectTransformLayerRef, TransformFunction: LLVM.OrcObjectTransformLayerTransformFunction, Ctx: Pointer<"Ctx">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1140 */
  export declare function OrcCreateLocalIndirectStubsManager(TargetTriple: Pointer<"TargetTriple">): LLVM.OrcIndirectStubsManagerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1145 */
  export declare function OrcDisposeIndirectStubsManager(ISM: LLVM.OrcIndirectStubsManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1147 */
  export declare function OrcCreateLocalLazyCallThroughManager(TargetTriple: Pointer<"TargetTriple">, ES: LLVM.OrcExecutionSessionRef, ErrorHandlerAddr: LLVM.OrcJITTargetAddress, LCTM: Pointer<"LCTM">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1155 */
  export declare function OrcDisposeLazyCallThroughManager(LCTM: LLVM.OrcLazyCallThroughManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1172 */
  export declare function OrcCreateDumpObjects(DumpDir: Pointer<"DumpDir">, IdentifierOverride: Pointer<"IdentifierOverride">): LLVM.OrcDumpObjectsRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1178 */
  export declare function OrcDisposeDumpObjects(DumpObjects: LLVM.OrcDumpObjectsRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Orc.h#L1183 */
  export declare function OrcDumpObjects_CallOperator(DumpObjects: LLVM.OrcDumpObjectsRef, ObjBuffer: Pointer<"ObjBuffer">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/OrcEE.h#L47 */
  export declare function OrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(ES: LLVM.OrcExecutionSessionRef): LLVM.OrcObjectLayerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/OrcEE.h#L56 */
  export declare function OrcRTDyldObjectLinkingLayerRegisterJITEventListener(RTDyldObjLinkingLayer: LLVM.OrcObjectLayerRef, Listener: LLVM.JITEventListenerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L32 */
  export declare function PassManagerBuilderCreate(): LLVM.PassManagerBuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L33 */
  export declare function PassManagerBuilderDispose(PMB: LLVM.PassManagerBuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L37 */
  export declare function PassManagerBuilderSetOptLevel(PMB: LLVM.PassManagerBuilderRef, OptLevel: Opaque<number, "OptLevel">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L42 */
  export declare function PassManagerBuilderSetSizeLevel(PMB: LLVM.PassManagerBuilderRef, SizeLevel: Opaque<number, "SizeLevel">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L47 */
  export declare function PassManagerBuilderSetDisableUnitAtATime(PMB: LLVM.PassManagerBuilderRef, Value: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L52 */
  export declare function PassManagerBuilderSetDisableUnrollLoops(PMB: LLVM.PassManagerBuilderRef, Value: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L57 */
  export declare function PassManagerBuilderSetDisableSimplifyLibCalls(PMB: LLVM.PassManagerBuilderRef, Value: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L62 */
  export declare function PassManagerBuilderUseInlinerWithThreshold(PMB: LLVM.PassManagerBuilderRef, Threshold: Opaque<number, "Threshold">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L67 */
  export declare function PassManagerBuilderPopulateFunctionPassManager(PMB: LLVM.PassManagerBuilderRef, PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassManagerBuilder.h#L72 */
  export declare function PassManagerBuilderPopulateModulePassManager(PMB: LLVM.PassManagerBuilderRef, PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Utils.h#L35 */
  export declare function AddLowerSwitchPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Utils.h#L38 */
  export declare function AddPromoteMemoryToRegisterPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Utils.h#L41 */
  export declare function AddAddDiscriminatorsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L31 */
  export declare function AddConstantMergePass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L34 */
  export declare function AddMergeFunctionsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L37 */
  export declare function AddCalledValuePropagationPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L40 */
  export declare function AddDeadArgEliminationPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L43 */
  export declare function AddFunctionAttrsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L46 */
  export declare function AddFunctionInliningPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L49 */
  export declare function AddAlwaysInlinerPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L52 */
  export declare function AddGlobalDCEPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L55 */
  export declare function AddGlobalOptimizerPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L58 */
  export declare function AddPruneEHPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L61 */
  export declare function AddIPSCCPPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L64 */
  export declare function AddInternalizePass(_0: LLVM.PassManagerRef, AllButMain: Opaque<number, "AllButMain">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L76 */
  export declare function AddInternalizePassWithMustPreservePredicate(PM: LLVM.PassManagerRef, Context: Pointer<"Context">, MustPreserve: FnPointer<"MustPreserve">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L82 */
  export declare function AddStripDeadPrototypesPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/IPO.h#L85 */
  export declare function AddStripSymbolsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L49 */
  export declare function RunPasses(M: LLVM.ModuleRef, Passes: Pointer<"Passes">, TM: LLVM.TargetMachineRef, Options: LLVM.PassBuilderOptionsRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L60 */
  export declare function CreatePassBuilderOptions(): LLVM.PassBuilderOptionsRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L66 */
  export declare function PassBuilderOptionsSetVerifyEach(Options: LLVM.PassBuilderOptionsRef, VerifyEach: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L72 */
  export declare function PassBuilderOptionsSetDebugLogging(Options: LLVM.PassBuilderOptionsRef, DebugLogging: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L75 */
  export declare function PassBuilderOptionsSetLoopInterleaving(Options: LLVM.PassBuilderOptionsRef, LoopInterleaving: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L78 */
  export declare function PassBuilderOptionsSetLoopVectorization(Options: LLVM.PassBuilderOptionsRef, LoopVectorization: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L81 */
  export declare function PassBuilderOptionsSetSLPVectorization(Options: LLVM.PassBuilderOptionsRef, SLPVectorization: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L84 */
  export declare function PassBuilderOptionsSetLoopUnrolling(Options: LLVM.PassBuilderOptionsRef, LoopUnrolling: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L87 */
  export declare function PassBuilderOptionsSetForgetAllSCEVInLoopUnroll(Options: LLVM.PassBuilderOptionsRef, ForgetAllSCEVInLoopUnroll: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L90 */
  export declare function PassBuilderOptionsSetLicmMssaOptCap(Options: LLVM.PassBuilderOptionsRef, LicmMssaOptCap: Opaque<number, "LicmMssaOptCap">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L93 */
  export declare function PassBuilderOptionsSetLicmMssaNoAccForPromotionCap(Options: LLVM.PassBuilderOptionsRef, LicmMssaNoAccForPromotionCap: Opaque<number, "LicmMssaNoAccForPromotionCap">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L96 */
  export declare function PassBuilderOptionsSetCallGraphProfile(Options: LLVM.PassBuilderOptionsRef, CallGraphProfile: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L99 */
  export declare function PassBuilderOptionsSetMergeFunctions(Options: LLVM.PassBuilderOptionsRef, MergeFunctions: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/PassBuilder.h#L105 */
  export declare function DisposePassBuilderOptions(Options: LLVM.PassBuilderOptionsRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/AggressiveInstCombine.h#L31 */
  export declare function AddAggressiveInstCombinerPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L35 */
  export declare function AddAggressiveDCEPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L38 */
  export declare function AddDCEPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L41 */
  export declare function AddBitTrackingDCEPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L44 */
  export declare function AddAlignmentFromAssumptionsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L47 */
  export declare function AddCFGSimplificationPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L50 */
  export declare function AddDeadStoreEliminationPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L53 */
  export declare function AddScalarizerPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L56 */
  export declare function AddMergedLoadStoreMotionPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L59 */
  export declare function AddGVNPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L62 */
  export declare function AddNewGVNPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L65 */
  export declare function AddIndVarSimplifyPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L68 */
  export declare function AddInstructionCombiningPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L71 */
  export declare function AddInstructionSimplifyPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L74 */
  export declare function AddJumpThreadingPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L77 */
  export declare function AddLICMPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L80 */
  export declare function AddLoopDeletionPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L83 */
  export declare function AddLoopIdiomPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L86 */
  export declare function AddLoopRotatePass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L89 */
  export declare function AddLoopRerollPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L92 */
  export declare function AddLoopUnrollPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L95 */
  export declare function AddLoopUnrollAndJamPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L98 */
  export declare function AddLowerAtomicPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L101 */
  export declare function AddMemCpyOptPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L104 */
  export declare function AddPartiallyInlineLibCallsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L107 */
  export declare function AddReassociatePass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L110 */
  export declare function AddSCCPPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L113 */
  export declare function AddScalarReplAggregatesPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L116 */
  export declare function AddScalarReplAggregatesPassSSA(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L119 */
  export declare function AddScalarReplAggregatesPassWithThreshold(PM: LLVM.PassManagerRef, Threshold: Opaque<number, "Threshold">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L123 */
  export declare function AddSimplifyLibCallsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L126 */
  export declare function AddTailCallEliminationPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L129 */
  export declare function AddDemoteMemoryToRegisterPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L132 */
  export declare function AddVerifierPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L135 */
  export declare function AddCorrelatedValuePropagationPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L138 */
  export declare function AddEarlyCSEPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L141 */
  export declare function AddEarlyCSEMemSSAPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L144 */
  export declare function AddLowerExpectIntrinsicPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L147 */
  export declare function AddLowerConstantIntrinsicsPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L150 */
  export declare function AddTypeBasedAliasAnalysisPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L153 */
  export declare function AddScopedNoAliasAAPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L156 */
  export declare function AddBasicAliasAnalysisPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Scalar.h#L159 */
  export declare function AddUnifyFunctionExitNodesPass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Vectorize.h#L36 */
  export declare function AddLoopVectorizePass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Transforms/Vectorize.h#L39 */
  export declare function AddSLPVectorizePass(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Linker.h#L41 */
  export declare function LinkModules2(Dest: LLVM.ModuleRef, Src: LLVM.ModuleRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L64 */
  export declare function RemarkStringGetData(String: LLVM.RemarkStringRef): Pointer<"LLVMRemarkStringGetData">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L71 */
  export declare function RemarkStringGetLen(String: LLVM.RemarkStringRef): Opaque<number, "LLVMRemarkStringGetLen">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L86 */
  export declare function RemarkDebugLocGetSourceFilePath(DL: LLVM.RemarkDebugLocRef): LLVM.RemarkStringRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L93 */
  export declare function RemarkDebugLocGetSourceLine(DL: LLVM.RemarkDebugLocRef): Opaque<number, "LLVMRemarkDebugLocGetSourceLine">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L100 */
  export declare function RemarkDebugLocGetSourceColumn(DL: LLVM.RemarkDebugLocRef): Opaque<number, "LLVMRemarkDebugLocGetSourceColumn">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L117 */
  export declare function RemarkArgGetKey(Arg: LLVM.RemarkArgRef): LLVM.RemarkStringRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L124 */
  export declare function RemarkArgGetValue(Arg: LLVM.RemarkArgRef): LLVM.RemarkStringRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L133 */
  export declare function RemarkArgGetDebugLoc(Arg: LLVM.RemarkArgRef): LLVM.RemarkDebugLocRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L147 */
  export declare function RemarkEntryDispose(Remark: LLVM.RemarkEntryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L155 */
  export declare function RemarkEntryGetType(Remark: LLVM.RemarkEntryRef): LLVM.RemarkType;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L163 */
  export declare function RemarkEntryGetPassName(Remark: LLVM.RemarkEntryRef): LLVM.RemarkStringRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L171 */
  export declare function RemarkEntryGetRemarkName(Remark: LLVM.RemarkEntryRef): LLVM.RemarkStringRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L179 */
  export declare function RemarkEntryGetFunctionName(Remark: LLVM.RemarkEntryRef): LLVM.RemarkStringRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L189 */
  export declare function RemarkEntryGetDebugLoc(Remark: LLVM.RemarkEntryRef): LLVM.RemarkDebugLocRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L198 */
  export declare function RemarkEntryGetHotness(Remark: LLVM.RemarkEntryRef): Opaque<bigint, "LLVMRemarkEntryGetHotness">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L205 */
  export declare function RemarkEntryGetNumArgs(Remark: LLVM.RemarkEntryRef): Opaque<number, "LLVMRemarkEntryGetNumArgs">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L216 */
  export declare function RemarkEntryGetFirstArg(Remark: LLVM.RemarkEntryRef): LLVM.RemarkArgRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L227 */
  export declare function RemarkEntryGetNextArg(It: LLVM.RemarkArgRef, Remark: LLVM.RemarkEntryRef): LLVM.RemarkArgRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L243 */
  export declare function RemarkParserCreateYAML(Buf: Pointer<"Buf">, Size: Opaque<bigint, "Size">): LLVM.RemarkParserRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L257 */
  export declare function RemarkParserCreateBitstream(Buf: Pointer<"Buf">, Size: Opaque<bigint, "Size">): LLVM.RemarkParserRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L302 */
  export declare function RemarkParserGetNext(Parser: LLVM.RemarkParserRef): LLVM.RemarkEntryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L309 */
  export declare function RemarkParserHasError(Parser: LLVM.RemarkParserRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L322 */
  export declare function RemarkParserGetErrorMessage(Parser: LLVM.RemarkParserRef): Pointer<"LLVMRemarkParserGetErrorMessage">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Remarks.h#L329 */
  export declare function RemarkParserDispose(Parser: LLVM.RemarkParserRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ErrorHandling.h#L36 */
  export declare function InstallFatalErrorHandler(Handler: LLVM.FatalErrorHandler): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ErrorHandling.h#L42 */
  export declare function ResetFatalErrorHandler(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/ErrorHandling.h#L49 */
  export declare function EnablePrettyStackTrace(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L475 */
  export declare function InitializeCore(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L480 */
  export declare function Shutdown(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L484 */
  export declare function CreateMessage(Message: Pointer<"Message">): Pointer<"LLVMCreateMessage">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L485 */
  export declare function DisposeMessage(Message: Pointer<"Message">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L508 */
  export declare function ContextCreate(): LLVM.ContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L513 */
  export declare function GetGlobalContext(): LLVM.ContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L518 */
  export declare function ContextSetDiagnosticHandler(C: LLVM.ContextRef, Handler: LLVM.DiagnosticHandler, DiagnosticContext: Pointer<"DiagnosticContext">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L525 */
  export declare function ContextGetDiagnosticHandler(C: LLVM.ContextRef): LLVM.DiagnosticHandler;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L530 */
  export declare function ContextGetDiagnosticContext(C: LLVM.ContextRef): Pointer<"LLVMContextGetDiagnosticContext">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L537 */
  export declare function ContextSetYieldCallback(C: LLVM.ContextRef, Callback: LLVM.YieldCallback, OpaqueHandle: Pointer<"OpaqueHandle">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L545 */
  export declare function ContextShouldDiscardValueNames(C: LLVM.ContextRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L555 */
  export declare function ContextSetDiscardValueNames(C: LLVM.ContextRef, Discard: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L562 */
  export declare function ContextSetOpaquePointers(C: LLVM.ContextRef, OpaquePointers: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L570 */
  export declare function ContextDispose(C: LLVM.ContextRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L578 */
  export declare function GetDiagInfoDescription(DI: LLVM.DiagnosticInfoRef): Pointer<"LLVMGetDiagInfoDescription">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L585 */
  export declare function GetDiagInfoSeverity(DI: LLVM.DiagnosticInfoRef): LLVM.DiagnosticSeverity;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L587 */
  export declare function GetMDKindIDInContext(C: LLVM.ContextRef, Name: Pointer<"Name">, SLen: Opaque<number, "SLen">): Opaque<number, "LLVMGetMDKindIDInContext">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L589 */
  export declare function GetMDKindID(Name: Pointer<"Name">, SLen: Opaque<number, "SLen">): Opaque<number, "LLVMGetMDKindID">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L602 */
  export declare function GetEnumAttributeKindForName(Name: Pointer<"Name">, SLen: Opaque<number, "SLen">): Opaque<number, "LLVMGetEnumAttributeKindForName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L603 */
  export declare function GetLastEnumAttributeKind(): Opaque<number, "LLVMGetLastEnumAttributeKind">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L608 */
  export declare function CreateEnumAttribute(C: LLVM.ContextRef, KindID: Opaque<number, "KindID">, Val: Opaque<bigint, "Val">): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L615 */
  export declare function GetEnumAttributeKind(A: LLVM.AttributeRef): Opaque<number, "LLVMGetEnumAttributeKind">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L620 */
  export declare function GetEnumAttributeValue(A: LLVM.AttributeRef): Opaque<bigint, "LLVMGetEnumAttributeValue">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L625 */
  export declare function CreateTypeAttribute(C: LLVM.ContextRef, KindID: Opaque<number, "KindID">, type_ref: LLVM.TypeRef): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L631 */
  export declare function GetTypeAttributeValue(A: LLVM.AttributeRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L636 */
  export declare function CreateStringAttribute(C: LLVM.ContextRef, K: Pointer<"K">, KLength: Opaque<number, "KLength">, V: Pointer<"V">, VLength: Opaque<number, "VLength">): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L643 */
  export declare function GetStringAttributeKind(A: LLVM.AttributeRef, Length: Pointer<"Length">): Pointer<"LLVMGetStringAttributeKind">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L648 */
  export declare function GetStringAttributeValue(A: LLVM.AttributeRef, Length: Pointer<"Length">): Pointer<"LLVMGetStringAttributeValue">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L653 */
  export declare function IsEnumAttribute(A: LLVM.AttributeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L654 */
  export declare function IsStringAttribute(A: LLVM.AttributeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L655 */
  export declare function IsTypeAttribute(A: LLVM.AttributeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L660 */
  export declare function GetTypeByName2(C: LLVM.ContextRef, Name: Pointer<"Name">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L685 */
  export declare function ModuleCreateWithName(ModuleID: Pointer<"ModuleID">): LLVM.ModuleRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L693 */
  export declare function ModuleCreateWithNameInContext(ModuleID: Pointer<"ModuleID">, C: LLVM.ContextRef): LLVM.ModuleRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L698 */
  export declare function CloneModule(M: LLVM.ModuleRef): LLVM.ModuleRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L706 */
  export declare function DisposeModule(M: LLVM.ModuleRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L716 */
  export declare function GetModuleIdentifier(M: LLVM.ModuleRef, Len: Pointer<"Len">): Pointer<"LLVMGetModuleIdentifier">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L726 */
  export declare function SetModuleIdentifier(M: LLVM.ModuleRef, Ident: Pointer<"Ident">, Len: Opaque<number, "Len">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L736 */
  export declare function GetSourceFileName(M: LLVM.ModuleRef, Len: Pointer<"Len">): Pointer<"LLVMGetSourceFileName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L747 */
  export declare function SetSourceFileName(M: LLVM.ModuleRef, Name: Pointer<"Name">, Len: Opaque<number, "Len">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L758 */
  export declare function GetDataLayoutStr(M: LLVM.ModuleRef): Pointer<"LLVMGetDataLayoutStr">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L759 */
  export declare function GetDataLayout(M: LLVM.ModuleRef): Pointer<"LLVMGetDataLayout">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L766 */
  export declare function SetDataLayout(M: LLVM.ModuleRef, DataLayoutStr: Pointer<"DataLayoutStr">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L773 */
  export declare function GetTarget(M: LLVM.ModuleRef): Pointer<"LLVMGetTarget">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L780 */
  export declare function SetTarget(M: LLVM.ModuleRef, Triple: Pointer<"Triple">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L789 */
  export declare function CopyModuleFlagsMetadata(M: LLVM.ModuleRef, Len: Pointer<"Len">): Pointer<"LLVMCopyModuleFlagsMetadata">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L794 */
  export declare function DisposeModuleFlagsMetadata(Entries: Pointer<"Entries">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L802 */
  export declare function ModuleFlagEntriesGetFlagBehavior(Entries: Pointer<"Entries">, Index: Opaque<number, "Index">): LLVM.ModuleFlagBehavior;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L810 */
  export declare function ModuleFlagEntriesGetKey(Entries: Pointer<"Entries">, Index: Opaque<number, "Index">, Len: Pointer<"Len">): Pointer<"LLVMModuleFlagEntriesGetKey">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L818 */
  export declare function ModuleFlagEntriesGetMetadata(Entries: Pointer<"Entries">, Index: Opaque<number, "Index">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L827 */
  export declare function GetModuleFlag(M: LLVM.ModuleRef, Key: Pointer<"Key">, KeyLen: Opaque<number, "KeyLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L836 */
  export declare function AddModuleFlag(M: LLVM.ModuleRef, Behavior: LLVM.ModuleFlagBehavior, Key: Pointer<"Key">, KeyLen: Opaque<number, "KeyLen">, Val: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L845 */
  export declare function DumpModule(M: LLVM.ModuleRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L853 */
  export declare function PrintModuleToFile(M: LLVM.ModuleRef, Filename: Pointer<"Filename">, ErrorMessage: Pointer<"ErrorMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L862 */
  export declare function PrintModuleToString(M: LLVM.ModuleRef): Pointer<"LLVMPrintModuleToString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L869 */
  export declare function GetModuleInlineAsm(M: LLVM.ModuleRef, Len: Pointer<"Len">): Pointer<"LLVMGetModuleInlineAsm">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L876 */
  export declare function SetModuleInlineAsm2(M: LLVM.ModuleRef, Asm: Pointer<"Asm">, Len: Opaque<number, "Len">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L883 */
  export declare function AppendModuleInlineAsm(M: LLVM.ModuleRef, Asm: Pointer<"Asm">, Len: Opaque<number, "Len">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L890 */
  export declare function GetInlineAsm(Ty: LLVM.TypeRef, AsmString: Pointer<"AsmString">, AsmStringSize: Opaque<number, "AsmStringSize">, Constraints: Pointer<"Constraints">, ConstraintsSize: Opaque<number, "ConstraintsSize">, HasSideEffects: LLVM.Bool, IsAlignStack: LLVM.Bool, Dialect: LLVM.InlineAsmDialect, CanThrow: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L901 */
  export declare function GetModuleContext(M: LLVM.ModuleRef): LLVM.ContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L904 */
  export declare function GetTypeByName(M: LLVM.ModuleRef, Name: Pointer<"Name">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L911 */
  export declare function GetFirstNamedMetadata(M: LLVM.ModuleRef): LLVM.NamedMDNodeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L918 */
  export declare function GetLastNamedMetadata(M: LLVM.ModuleRef): LLVM.NamedMDNodeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L926 */
  export declare function GetNextNamedMetadata(NamedMDNode: LLVM.NamedMDNodeRef): LLVM.NamedMDNodeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L934 */
  export declare function GetPreviousNamedMetadata(NamedMDNode: LLVM.NamedMDNodeRef): LLVM.NamedMDNodeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L942 */
  export declare function GetNamedMetadata(M: LLVM.ModuleRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): LLVM.NamedMDNodeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L951 */
  export declare function GetOrInsertNamedMetadata(M: LLVM.ModuleRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): LLVM.NamedMDNodeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L960 */
  export declare function GetNamedMetadataName(NamedMD: LLVM.NamedMDNodeRef, NameLen: Pointer<"NameLen">): Pointer<"LLVMGetNamedMetadataName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L968 */
  export declare function GetNamedMetadataNumOperands(M: LLVM.ModuleRef, Name: Pointer<"Name">): Opaque<number, "LLVMGetNamedMetadataNumOperands">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L981 */
  export declare function GetNamedMetadataOperands(M: LLVM.ModuleRef, Name: Pointer<"Name">, Dest: Pointer<"Dest">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L990 */
  export declare function AddNamedMetadataOperand(M: LLVM.ModuleRef, Name: Pointer<"Name">, Val: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1001 */
  export declare function GetDebugLocDirectory(Val: LLVM.ValueRef, Length: Pointer<"Length">): Pointer<"LLVMGetDebugLocDirectory">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1011 */
  export declare function GetDebugLocFilename(Val: LLVM.ValueRef, Length: Pointer<"Length">): Pointer<"LLVMGetDebugLocFilename">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1021 */
  export declare function GetDebugLocLine(Val: LLVM.ValueRef): Opaque<number, "LLVMGetDebugLocLine">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1029 */
  export declare function GetDebugLocColumn(Val: LLVM.ValueRef): Opaque<number, "LLVMGetDebugLocColumn">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1036 */
  export declare function AddFunction(M: LLVM.ModuleRef, Name: Pointer<"Name">, FunctionTy: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1046 */
  export declare function GetNamedFunction(M: LLVM.ModuleRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1053 */
  export declare function GetFirstFunction(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1060 */
  export declare function GetLastFunction(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1068 */
  export declare function GetNextFunction(Fn: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1076 */
  export declare function GetPreviousFunction(Fn: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1079 */
  export declare function SetModuleInlineAsm(M: LLVM.ModuleRef, Asm: Pointer<"Asm">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1119 */
  export declare function GetTypeKind(Ty: LLVM.TypeRef): LLVM.TypeKind;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1128 */
  export declare function TypeIsSized(Ty: LLVM.TypeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1135 */
  export declare function GetTypeContext(Ty: LLVM.TypeRef): LLVM.ContextRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1142 */
  export declare function DumpType(Val: LLVM.TypeRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1150 */
  export declare function PrintTypeToString(Val: LLVM.TypeRef): Pointer<"LLVMPrintTypeToString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1163 */
  export declare function Int1TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1164 */
  export declare function Int8TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1165 */
  export declare function Int16TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1166 */
  export declare function Int32TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1167 */
  export declare function Int64TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1168 */
  export declare function Int128TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1169 */
  export declare function IntTypeInContext(C: LLVM.ContextRef, NumBits: Opaque<number, "NumBits">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1175 */
  export declare function Int1Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1176 */
  export declare function Int8Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1177 */
  export declare function Int16Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1178 */
  export declare function Int32Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1179 */
  export declare function Int64Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1180 */
  export declare function Int128Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1181 */
  export declare function IntType(NumBits: Opaque<number, "NumBits">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1182 */
  export declare function GetIntTypeWidth(IntegerTy: LLVM.TypeRef): Opaque<number, "LLVMGetIntTypeWidth">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1197 */
  export declare function HalfTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1202 */
  export declare function BFloatTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1207 */
  export declare function FloatTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1212 */
  export declare function DoubleTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1217 */
  export declare function X86FP80TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1223 */
  export declare function FP128TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1228 */
  export declare function PPCFP128TypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1235 */
  export declare function HalfType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1236 */
  export declare function BFloatType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1237 */
  export declare function FloatType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1238 */
  export declare function DoubleType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1239 */
  export declare function X86FP80Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1240 */
  export declare function FP128Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1241 */
  export declare function PPCFP128Type(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1259 */
  export declare function FunctionType(ReturnType: LLVM.TypeRef, ParamTypes: Pointer<"ParamTypes">, ParamCount: Opaque<number, "ParamCount">, IsVarArg: LLVM.Bool): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1266 */
  export declare function IsFunctionVarArg(FunctionTy: LLVM.TypeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1271 */
  export declare function GetReturnType(FunctionTy: LLVM.TypeRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1276 */
  export declare function CountParamTypes(FunctionTy: LLVM.TypeRef): Opaque<number, "LLVMCountParamTypes">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1289 */
  export declare function GetParamTypes(FunctionTy: LLVM.TypeRef, Dest: Pointer<"Dest">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1313 */
  export declare function StructTypeInContext(C: LLVM.ContextRef, ElementTypes: Pointer<"ElementTypes">, ElementCount: Opaque<number, "ElementCount">, Packed: LLVM.Bool): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1321 */
  export declare function StructType(ElementTypes: Pointer<"ElementTypes">, ElementCount: Opaque<number, "ElementCount">, Packed: LLVM.Bool): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1329 */
  export declare function StructCreateNamed(C: LLVM.ContextRef, Name: Pointer<"Name">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1336 */
  export declare function GetStructName(Ty: LLVM.TypeRef): Pointer<"LLVMGetStructName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1343 */
  export declare function StructSetBody(StructTy: LLVM.TypeRef, ElementTypes: Pointer<"ElementTypes">, ElementCount: Opaque<number, "ElementCount">, Packed: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1351 */
  export declare function CountStructElementTypes(StructTy: LLVM.TypeRef): Opaque<number, "LLVMCountStructElementTypes">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1363 */
  export declare function GetStructElementTypes(StructTy: LLVM.TypeRef, Dest: Pointer<"Dest">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1370 */
  export declare function StructGetTypeAtIndex(StructTy: LLVM.TypeRef, i: Opaque<number, "i">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1377 */
  export declare function IsPackedStruct(StructTy: LLVM.TypeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1384 */
  export declare function IsOpaqueStruct(StructTy: LLVM.TypeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1391 */
  export declare function IsLiteralStruct(StructTy: LLVM.TypeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1413 */
  export declare function GetElementType(Ty: LLVM.TypeRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1420 */
  export declare function GetSubtypes(Tp: LLVM.TypeRef, Arr: Pointer<"Arr">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1427 */
  export declare function GetNumContainedTypes(Tp: LLVM.TypeRef): Opaque<number, "LLVMGetNumContainedTypes">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1437 */
  export declare function ArrayType(ElementType: LLVM.TypeRef, ElementCount: Opaque<number, "ElementCount">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1446 */
  export declare function GetArrayLength(ArrayTy: LLVM.TypeRef): Opaque<number, "LLVMGetArrayLength">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1456 */
  export declare function PointerType(ElementType: LLVM.TypeRef, AddressSpace: Opaque<number, "AddressSpace">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1465 */
  export declare function PointerTypeIsOpaque(Ty: LLVM.TypeRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1472 */
  export declare function PointerTypeInContext(C: LLVM.ContextRef, AddressSpace: Opaque<number, "AddressSpace">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1481 */
  export declare function GetPointerAddressSpace(PointerTy: LLVM.TypeRef): Opaque<number, "LLVMGetPointerAddressSpace">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1492 */
  export declare function VectorType(ElementType: LLVM.TypeRef, ElementCount: Opaque<number, "ElementCount">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1503 */
  export declare function ScalableVectorType(ElementType: LLVM.TypeRef, ElementCount: Opaque<number, "ElementCount">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1513 */
  export declare function GetVectorSize(VectorTy: LLVM.TypeRef): Opaque<number, "LLVMGetVectorSize">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1528 */
  export declare function VoidTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1533 */
  export declare function LabelTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1538 */
  export declare function X86MMXTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1543 */
  export declare function X86AMXTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1548 */
  export declare function TokenTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1553 */
  export declare function MetadataTypeInContext(C: LLVM.ContextRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1559 */
  export declare function VoidType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1560 */
  export declare function LabelType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1561 */
  export declare function X86MMXType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1562 */
  export declare function X86AMXType(): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1698 */
  export declare function TypeOf(Val: LLVM.ValueRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1705 */
  export declare function GetValueKind(Val: LLVM.ValueRef): LLVM.ValueKind;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1712 */
  export declare function GetValueName2(Val: LLVM.ValueRef, Length: Pointer<"Length">): Pointer<"LLVMGetValueName2">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1719 */
  export declare function SetValueName2(Val: LLVM.ValueRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1726 */
  export declare function DumpValue(Val: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1734 */
  export declare function PrintValueToString(Val: LLVM.ValueRef): Pointer<"LLVMPrintValueToString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1741 */
  export declare function ReplaceAllUsesWith(OldVal: LLVM.ValueRef, NewVal: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1746 */
  export declare function IsConstant(Val: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1751 */
  export declare function IsUndef(Val: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1756 */
  export declare function IsPoison(Val: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAArgument(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsABasicBlock(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAInlineAsm(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAUser(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstant(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsABlockAddress(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantAggregateZero(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantArray(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantDataSequential(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantDataArray(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantDataVector(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantExpr(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantFP(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantInt(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantPointerNull(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantStruct(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantTokenNone(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAConstantVector(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAGlobalValue(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAGlobalAlias(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAGlobalObject(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFunction(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAGlobalVariable(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAGlobalIFunc(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAUndefValue(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAPoisonValue(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAInstruction(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAUnaryOperator(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsABinaryOperator(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACallInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAIntrinsicInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsADbgInfoIntrinsic(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsADbgVariableIntrinsic(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsADbgDeclareInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsADbgLabelInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAMemIntrinsic(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAMemCpyInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAMemMoveInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAMemSetInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACmpInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFCmpInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAICmpInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAExtractElementInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAGetElementPtrInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAInsertElementInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAInsertValueInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsALandingPadInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAPHINode(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsASelectInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAShuffleVectorInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAStoreInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsABranchInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAIndirectBrInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAInvokeInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAReturnInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsASwitchInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAUnreachableInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAResumeInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACleanupReturnInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACatchReturnInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACatchSwitchInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACallBrInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFuncletPadInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACatchPadInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACleanupPadInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAUnaryInstruction(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAAllocaInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsACastInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAAddrSpaceCastInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsABitCastInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFPExtInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFPToSIInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFPToUIInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFPTruncInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAIntToPtrInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAPtrToIntInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsASExtInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsASIToFPInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsATruncInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAUIToFPInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAZExtInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAExtractValueInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsALoadInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAVAArgInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFreezeInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAAtomicCmpXchgInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAAtomicRMWInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1771 */
  export declare function IsAFenceInst(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1773 */
  export declare function IsAMDNode(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1774 */
  export declare function IsAMDString(Val: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1777 */
  export declare function GetValueName(Val: LLVM.ValueRef): Pointer<"LLVMGetValueName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1779 */
  export declare function SetValueName(Val: LLVM.ValueRef, Name: Pointer<"Name">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1808 */
  export declare function GetFirstUse(Val: LLVM.ValueRef): LLVM.UseRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1816 */
  export declare function GetNextUse(U: LLVM.UseRef): LLVM.UseRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1825 */
  export declare function GetUser(U: LLVM.UseRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1832 */
  export declare function GetUsedValue(U: LLVM.UseRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1853 */
  export declare function GetOperand(Val: LLVM.ValueRef, Index: Opaque<number, "Index">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1860 */
  export declare function GetOperandUse(Val: LLVM.ValueRef, Index: Opaque<number, "Index">): LLVM.UseRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1867 */
  export declare function SetOperand(User: LLVM.ValueRef, Index: Opaque<number, "Index">, Val: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1874 */
  export declare function GetNumOperands(Val: LLVM.ValueRef): Opaque<number, "LLVMGetNumOperands">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1897 */
  export declare function ConstNull(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1907 */
  export declare function ConstAllOnes(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1914 */
  export declare function GetUndef(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1921 */
  export declare function GetPoison(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1928 */
  export declare function IsNull(Val: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1934 */
  export declare function ConstPointerNull(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1963 */
  export declare function ConstInt(IntTy: LLVM.TypeRef, N: Opaque<bigint, "N">, SignExtend: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1971 */
  export declare function ConstIntOfArbitraryPrecision(IntTy: LLVM.TypeRef, NumWords: Opaque<number, "NumWords">, Words: Pointer<"Words">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1984 */
  export declare function ConstIntOfString(IntTy: LLVM.TypeRef, Text: Pointer<"Text">, Radix: Opaque<number, "Radix">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1993 */
  export declare function ConstIntOfStringAndSize(IntTy: LLVM.TypeRef, Text: Pointer<"Text">, SLen: Opaque<number, "SLen">, Radix: Opaque<number, "Radix">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L1999 */
  export declare function ConstReal(RealTy: LLVM.TypeRef, N: Opaque<bigint, "N">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2007 */
  export declare function ConstRealOfString(RealTy: LLVM.TypeRef, Text: Pointer<"Text">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2012 */
  export declare function ConstRealOfStringAndSize(RealTy: LLVM.TypeRef, Text: Pointer<"Text">, SLen: Opaque<number, "SLen">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2020 */
  export declare function ConstIntGetZExtValue(ConstantVal: LLVM.ValueRef): Opaque<bigint, "LLVMConstIntGetZExtValue">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2027 */
  export declare function ConstIntGetSExtValue(ConstantVal: LLVM.ValueRef): Opaque<bigint, "LLVMConstIntGetSExtValue">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2035 */
  export declare function ConstRealGetDouble(ConstantVal: LLVM.ValueRef, losesInfo: Pointer<"losesInfo">): Opaque<bigint, "LLVMConstRealGetDouble">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2054 */
  export declare function ConstStringInContext(C: LLVM.ContextRef, Str: Pointer<"Str">, Length: Opaque<number, "Length">, DontNullTerminate: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2066 */
  export declare function ConstString(Str: Pointer<"Str">, Length: Opaque<number, "Length">, DontNullTerminate: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2074 */
  export declare function IsConstantString(c: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2081 */
  export declare function GetAsString(c: LLVM.ValueRef, Length: Pointer<"Length">): Pointer<"LLVMGetAsString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2088 */
  export declare function ConstStructInContext(C: LLVM.ContextRef, ConstantVals: Pointer<"ConstantVals">, Count: Opaque<number, "Count">, Packed: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2100 */
  export declare function ConstStruct(ConstantVals: Pointer<"ConstantVals">, Count: Opaque<number, "Count">, Packed: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2108 */
  export declare function ConstArray(ElementTy: LLVM.TypeRef, ConstantVals: Pointer<"ConstantVals">, Length: Opaque<number, "Length">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2116 */
  export declare function ConstNamedStruct(StructTy: LLVM.TypeRef, ConstantVals: Pointer<"ConstantVals">, Count: Opaque<number, "Count">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2128 */
  export declare function GetAggregateElement(C: LLVM.ValueRef, Idx: Opaque<number, "Idx">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2135 */
  export declare function GetElementAsConstant(C: LLVM.ValueRef, idx: Opaque<number, "idx">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2144 */
  export declare function ConstVector(ScalarConstantVals: Pointer<"ScalarConstantVals">, Size: Opaque<number, "Size">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2159 */
  export declare function GetConstOpcode(ConstantVal: LLVM.ValueRef): LLVM.Opcode;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2160 */
  export declare function AlignOf(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2161 */
  export declare function SizeOf(Ty: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2162 */
  export declare function ConstNeg(ConstantVal: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2163 */
  export declare function ConstNSWNeg(ConstantVal: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2164 */
  export declare function ConstNUWNeg(ConstantVal: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2165 */
  export declare function ConstFNeg(ConstantVal: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2166 */
  export declare function ConstNot(ConstantVal: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2167 */
  export declare function ConstAdd(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2168 */
  export declare function ConstNSWAdd(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2169 */
  export declare function ConstNUWAdd(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2170 */
  export declare function ConstSub(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2171 */
  export declare function ConstNSWSub(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2172 */
  export declare function ConstNUWSub(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2173 */
  export declare function ConstMul(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2174 */
  export declare function ConstNSWMul(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2175 */
  export declare function ConstNUWMul(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2176 */
  export declare function ConstAnd(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2177 */
  export declare function ConstOr(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2178 */
  export declare function ConstXor(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2179 */
  export declare function ConstICmp(Predicate: LLVM.IntPredicate, LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2181 */
  export declare function ConstFCmp(Predicate: LLVM.RealPredicate, LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2183 */
  export declare function ConstShl(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2184 */
  export declare function ConstLShr(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2185 */
  export declare function ConstAShr(LHSConstant: LLVM.ValueRef, RHSConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2186 */
  export declare function ConstGEP(ConstantVal: LLVM.ValueRef, ConstantIndices: Pointer<"ConstantIndices">, NumIndices: Opaque<number, "NumIndices">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2191 */
  export declare function ConstGEP2(Ty: LLVM.TypeRef, ConstantVal: LLVM.ValueRef, ConstantIndices: Pointer<"ConstantIndices">, NumIndices: Opaque<number, "NumIndices">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2193 */
  export declare function ConstInBoundsGEP(ConstantVal: LLVM.ValueRef, ConstantIndices: Pointer<"ConstantIndices">, NumIndices: Opaque<number, "NumIndices">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2198 */
  export declare function ConstInBoundsGEP2(Ty: LLVM.TypeRef, ConstantVal: LLVM.ValueRef, ConstantIndices: Pointer<"ConstantIndices">, NumIndices: Opaque<number, "NumIndices">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2201 */
  export declare function ConstTrunc(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2202 */
  export declare function ConstSExt(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2203 */
  export declare function ConstZExt(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2204 */
  export declare function ConstFPTrunc(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2205 */
  export declare function ConstFPExt(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2206 */
  export declare function ConstUIToFP(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2207 */
  export declare function ConstSIToFP(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2208 */
  export declare function ConstFPToUI(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2209 */
  export declare function ConstFPToSI(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2210 */
  export declare function ConstPtrToInt(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2211 */
  export declare function ConstIntToPtr(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2212 */
  export declare function ConstBitCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2213 */
  export declare function ConstAddrSpaceCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2214 */
  export declare function ConstZExtOrBitCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2216 */
  export declare function ConstSExtOrBitCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2218 */
  export declare function ConstTruncOrBitCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2220 */
  export declare function ConstPointerCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2222 */
  export declare function ConstIntCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef, isSigned: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2224 */
  export declare function ConstFPCast(ConstantVal: LLVM.ValueRef, ToType: LLVM.TypeRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2225 */
  export declare function ConstSelect(ConstantCondition: LLVM.ValueRef, ConstantIfTrue: LLVM.ValueRef, ConstantIfFalse: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2228 */
  export declare function ConstExtractElement(VectorConstant: LLVM.ValueRef, IndexConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2230 */
  export declare function ConstInsertElement(VectorConstant: LLVM.ValueRef, ElementValueConstant: LLVM.ValueRef, IndexConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2233 */
  export declare function ConstShuffleVector(VectorAConstant: LLVM.ValueRef, VectorBConstant: LLVM.ValueRef, MaskConstant: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2236 */
  export declare function BlockAddress(F: LLVM.ValueRef, BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2239 */
  export declare function ConstInlineAsm(Ty: LLVM.TypeRef, AsmString: Pointer<"AsmString">, Constraints: Pointer<"Constraints">, HasSideEffects: LLVM.Bool, IsAlignStack: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2258 */
  export declare function GetGlobalParent(Global: LLVM.ValueRef): LLVM.ModuleRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2259 */
  export declare function IsDeclaration(Global: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2260 */
  export declare function GetLinkage(Global: LLVM.ValueRef): LLVM.Linkage;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2261 */
  export declare function SetLinkage(Global: LLVM.ValueRef, Linkage: LLVM.Linkage): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2262 */
  export declare function GetSection(Global: LLVM.ValueRef): Pointer<"LLVMGetSection">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2263 */
  export declare function SetSection(Global: LLVM.ValueRef, Section: Pointer<"Section">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2264 */
  export declare function GetVisibility(Global: LLVM.ValueRef): LLVM.Visibility;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2265 */
  export declare function SetVisibility(Global: LLVM.ValueRef, Viz: LLVM.Visibility): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2266 */
  export declare function GetDLLStorageClass(Global: LLVM.ValueRef): LLVM.DLLStorageClass;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2267 */
  export declare function SetDLLStorageClass(Global: LLVM.ValueRef, Class: LLVM.DLLStorageClass): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2268 */
  export declare function GetUnnamedAddress(Global: LLVM.ValueRef): LLVM.UnnamedAddr;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2269 */
  export declare function SetUnnamedAddress(Global: LLVM.ValueRef, UnnamedAddr: LLVM.UnnamedAddr): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2277 */
  export declare function GlobalGetValueType(Global: LLVM.ValueRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2280 */
  export declare function HasUnnamedAddr(Global: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2282 */
  export declare function SetUnnamedAddr(Global: LLVM.ValueRef, HasUnnamedAddr: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2300 */
  export declare function GetAlignment(V: LLVM.ValueRef): Opaque<number, "LLVMGetAlignment">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2311 */
  export declare function SetAlignment(V: LLVM.ValueRef, Bytes: Opaque<number, "Bytes">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2319 */
  export declare function GlobalSetMetadata(Global: LLVM.ValueRef, Kind: Opaque<number, "Kind">, MD: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2327 */
  export declare function GlobalEraseMetadata(Global: LLVM.ValueRef, Kind: Opaque<number, "Kind">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2334 */
  export declare function GlobalClearMetadata(Global: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2343 */
  export declare function GlobalCopyAllMetadata(Value: LLVM.ValueRef, NumEntries: Pointer<"NumEntries">): Pointer<"LLVMGlobalCopyAllMetadata">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2349 */
  export declare function DisposeValueMetadataEntries(Entries: Pointer<"Entries">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2354 */
  export declare function ValueMetadataEntriesGetKind(Entries: Pointer<"Entries">, Index: Opaque<number, "Index">): Opaque<number, "LLVMValueMetadataEntriesGetKind">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2362 */
  export declare function ValueMetadataEntriesGetMetadata(Entries: Pointer<"Entries">, Index: Opaque<number, "Index">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2378 */
  export declare function AddGlobal(M: LLVM.ModuleRef, Ty: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2379 */
  export declare function AddGlobalInAddressSpace(M: LLVM.ModuleRef, Ty: LLVM.TypeRef, Name: Pointer<"Name">, AddressSpace: Opaque<number, "AddressSpace">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2382 */
  export declare function GetNamedGlobal(M: LLVM.ModuleRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2383 */
  export declare function GetFirstGlobal(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2384 */
  export declare function GetLastGlobal(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2385 */
  export declare function GetNextGlobal(GlobalVar: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2386 */
  export declare function GetPreviousGlobal(GlobalVar: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2387 */
  export declare function DeleteGlobal(GlobalVar: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2388 */
  export declare function GetInitializer(GlobalVar: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2389 */
  export declare function SetInitializer(GlobalVar: LLVM.ValueRef, ConstantVal: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2390 */
  export declare function IsThreadLocal(GlobalVar: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2391 */
  export declare function SetThreadLocal(GlobalVar: LLVM.ValueRef, IsThreadLocal: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2392 */
  export declare function IsGlobalConstant(GlobalVar: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2393 */
  export declare function SetGlobalConstant(GlobalVar: LLVM.ValueRef, IsConstant: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2394 */
  export declare function GetThreadLocalMode(GlobalVar: LLVM.ValueRef): LLVM.ThreadLocalMode;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2395 */
  export declare function SetThreadLocalMode(GlobalVar: LLVM.ValueRef, Mode: LLVM.ThreadLocalMode): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2396 */
  export declare function IsExternallyInitialized(GlobalVar: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2397 */
  export declare function SetExternallyInitialized(GlobalVar: LLVM.ValueRef, IsExtInit: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2413 */
  export declare function AddAlias(M: LLVM.ModuleRef, Ty: LLVM.TypeRef, Aliasee: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2423 */
  export declare function AddAlias2(M: LLVM.ModuleRef, ValueTy: LLVM.TypeRef, AddrSpace: Opaque<number, "AddrSpace">, Aliasee: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2434 */
  export declare function GetNamedGlobalAlias(M: LLVM.ModuleRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2442 */
  export declare function GetFirstGlobalAlias(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2449 */
  export declare function GetLastGlobalAlias(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2457 */
  export declare function GetNextGlobalAlias(GA: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2465 */
  export declare function GetPreviousGlobalAlias(GA: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2470 */
  export declare function AliasGetAliasee(Alias: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2475 */
  export declare function AliasSetAliasee(Alias: LLVM.ValueRef, Aliasee: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2497 */
  export declare function DeleteFunction(Fn: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2504 */
  export declare function HasPersonalityFn(Fn: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2511 */
  export declare function GetPersonalityFn(Fn: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2518 */
  export declare function SetPersonalityFn(Fn: LLVM.ValueRef, PersonalityFn: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2525 */
  export declare function LookupIntrinsicID(Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): Opaque<number, "LLVMLookupIntrinsicID">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2532 */
  export declare function GetIntrinsicID(Fn: LLVM.ValueRef): Opaque<number, "LLVMGetIntrinsicID">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2540 */
  export declare function GetIntrinsicDeclaration(Mod: LLVM.ModuleRef, ID: Opaque<number, "ID">, ParamTypes: Pointer<"ParamTypes">, ParamCount: Opaque<number, "ParamCount">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2551 */
  export declare function IntrinsicGetType(Ctx: LLVM.ContextRef, ID: Opaque<number, "ID">, ParamTypes: Pointer<"ParamTypes">, ParamCount: Opaque<number, "ParamCount">): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2559 */
  export declare function IntrinsicGetName(ID: Opaque<number, "ID">, NameLength: Pointer<"NameLength">): Pointer<"LLVMIntrinsicGetName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2562 */
  export declare function IntrinsicCopyOverloadedName(ID: Opaque<number, "ID">, ParamTypes: Pointer<"ParamTypes">, ParamCount: Opaque<number, "ParamCount">, NameLength: Pointer<"NameLength">): Pointer<"LLVMIntrinsicCopyOverloadedName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2578 */
  export declare function IntrinsicCopyOverloadedName2(Mod: LLVM.ModuleRef, ID: Opaque<number, "ID">, ParamTypes: Pointer<"ParamTypes">, ParamCount: Opaque<number, "ParamCount">, NameLength: Pointer<"NameLength">): Pointer<"LLVMIntrinsicCopyOverloadedName2">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2588 */
  export declare function IntrinsicIsOverloaded(ID: Opaque<number, "ID">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2597 */
  export declare function GetFunctionCallConv(Fn: LLVM.ValueRef): Opaque<number, "LLVMGetFunctionCallConv">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2607 */
  export declare function SetFunctionCallConv(Fn: LLVM.ValueRef, CC: Opaque<number, "CC">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2615 */
  export declare function GetGC(Fn: LLVM.ValueRef): Pointer<"LLVMGetGC">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2622 */
  export declare function SetGC(Fn: LLVM.ValueRef, Name: Pointer<"Name">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2629 */
  export declare function AddAttributeAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex, A: LLVM.AttributeRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2631 */
  export declare function GetAttributeCountAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex): Opaque<number, "LLVMGetAttributeCountAtIndex">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2632 */
  export declare function GetAttributesAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex, Attrs: Pointer<"Attrs">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2634 */
  export declare function GetEnumAttributeAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex, KindID: Opaque<number, "KindID">): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2637 */
  export declare function GetStringAttributeAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex, K: Pointer<"K">, KLen: Opaque<number, "KLen">): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2640 */
  export declare function RemoveEnumAttributeAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex, KindID: Opaque<number, "KindID">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2642 */
  export declare function RemoveStringAttributeAtIndex(F: LLVM.ValueRef, Idx: LLVM.AttributeIndex, K: Pointer<"K">, KLen: Opaque<number, "KLen">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2649 */
  export declare function AddTargetDependentFunctionAttr(Fn: LLVM.ValueRef, A: Pointer<"A">, V: Pointer<"V">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2668 */
  export declare function CountParams(Fn: LLVM.ValueRef): Opaque<number, "LLVMCountParams">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2681 */
  export declare function GetParams(Fn: LLVM.ValueRef, Params: Pointer<"Params">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2690 */
  export declare function GetParam(Fn: LLVM.ValueRef, Index: Opaque<number, "Index">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2701 */
  export declare function GetParamParent(Inst: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2708 */
  export declare function GetFirstParam(Fn: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2715 */
  export declare function GetLastParam(Fn: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2724 */
  export declare function GetNextParam(Arg: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2731 */
  export declare function GetPreviousParam(Arg: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2739 */
  export declare function SetParamAlignment(Arg: LLVM.ValueRef, Align: Opaque<number, "Align">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2761 */
  export declare function AddGlobalIFunc(M: LLVM.ModuleRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">, Ty: LLVM.TypeRef, AddrSpace: Opaque<number, "AddrSpace">, Resolver: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2773 */
  export declare function GetNamedGlobalIFunc(M: LLVM.ModuleRef, Name: Pointer<"Name">, NameLen: Opaque<number, "NameLen">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2781 */
  export declare function GetFirstGlobalIFunc(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2788 */
  export declare function GetLastGlobalIFunc(M: LLVM.ModuleRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2796 */
  export declare function GetNextGlobalIFunc(IFunc: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2804 */
  export declare function GetPreviousGlobalIFunc(IFunc: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2812 */
  export declare function GetGlobalIFuncResolver(IFunc: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2819 */
  export declare function SetGlobalIFuncResolver(IFunc: LLVM.ValueRef, Resolver: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2826 */
  export declare function EraseGlobalIFunc(IFunc: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2836 */
  export declare function RemoveGlobalIFunc(IFunc: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2868 */
  export declare function MDStringInContext2(C: LLVM.ContextRef, Str: Pointer<"Str">, SLen: Opaque<number, "SLen">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2876 */
  export declare function MDNodeInContext2(C: LLVM.ContextRef, MDs: Pointer<"MDs">, Count: Opaque<number, "Count">): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2882 */
  export declare function MetadataAsValue(C: LLVM.ContextRef, MD: LLVM.MetadataRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2887 */
  export declare function ValueAsMetadata(Val: LLVM.ValueRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2896 */
  export declare function GetMDString(V: LLVM.ValueRef, Length: Pointer<"Length">): Pointer<"LLVMGetMDString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2904 */
  export declare function GetMDNodeNumOperands(V: LLVM.ValueRef): Opaque<number, "LLVMGetMDNodeNumOperands">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2917 */
  export declare function GetMDNodeOperands(V: LLVM.ValueRef, Dest: Pointer<"Dest">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2920 */
  export declare function MDStringInContext(C: LLVM.ContextRef, Str: Pointer<"Str">, SLen: Opaque<number, "SLen">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2923 */
  export declare function MDString(Str: Pointer<"Str">, SLen: Opaque<number, "SLen">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2925 */
  export declare function MDNodeInContext(C: LLVM.ContextRef, Vals: Pointer<"Vals">, Count: Opaque<number, "Count">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2928 */
  export declare function MDNode(Vals: Pointer<"Vals">, Count: Opaque<number, "Count">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2954 */
  export declare function BasicBlockAsValue(BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2959 */
  export declare function ValueIsBasicBlock(Val: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2964 */
  export declare function ValueAsBasicBlock(Val: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2969 */
  export declare function GetBasicBlockName(BB: LLVM.BasicBlockRef): Pointer<"LLVMGetBasicBlockName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2976 */
  export declare function GetBasicBlockParent(BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2988 */
  export declare function GetBasicBlockTerminator(BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L2995 */
  export declare function CountBasicBlocks(Fn: LLVM.ValueRef): Opaque<number, "LLVMCountBasicBlocks">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3005 */
  export declare function GetBasicBlocks(Fn: LLVM.ValueRef, BasicBlocks: Pointer<"BasicBlocks">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3015 */
  export declare function GetFirstBasicBlock(Fn: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3022 */
  export declare function GetLastBasicBlock(Fn: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3027 */
  export declare function GetNextBasicBlock(BB: LLVM.BasicBlockRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3032 */
  export declare function GetPreviousBasicBlock(BB: LLVM.BasicBlockRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3040 */
  export declare function GetEntryBasicBlock(Fn: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3049 */
  export declare function InsertExistingBasicBlockAfterInsertBlock(Builder: LLVM.BuilderRef, BB: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3057 */
  export declare function AppendExistingBasicBlock(Fn: LLVM.ValueRef, BB: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3065 */
  export declare function CreateBasicBlockInContext(C: LLVM.ContextRef, Name: Pointer<"Name">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3073 */
  export declare function AppendBasicBlockInContext(C: LLVM.ContextRef, Fn: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3083 */
  export declare function AppendBasicBlock(Fn: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3093 */
  export declare function InsertBasicBlockInContext(C: LLVM.ContextRef, BB: LLVM.BasicBlockRef, Name: Pointer<"Name">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3102 */
  export declare function InsertBasicBlock(InsertBeforeBB: LLVM.BasicBlockRef, Name: Pointer<"Name">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3113 */
  export declare function DeleteBasicBlock(BB: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3123 */
  export declare function RemoveBasicBlockFromParent(BB: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3130 */
  export declare function MoveBasicBlockBefore(BB: LLVM.BasicBlockRef, MovePos: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3137 */
  export declare function MoveBasicBlockAfter(BB: LLVM.BasicBlockRef, MovePos: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3145 */
  export declare function GetFirstInstruction(BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3152 */
  export declare function GetLastInstruction(BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3178 */
  export declare function HasMetadata(Val: LLVM.ValueRef): Opaque<number, "LLVMHasMetadata">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3183 */
  export declare function GetMetadata(Val: LLVM.ValueRef, KindID: Opaque<number, "KindID">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3188 */
  export declare function SetMetadata(Val: LLVM.ValueRef, KindID: Opaque<number, "KindID">, Node: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3197 */
  export declare function InstructionGetAllMetadataOtherThanDebugLoc(Instr: LLVM.ValueRef, NumEntries: Pointer<"NumEntries">): Pointer<"LLVMInstructionGetAllMetadataOtherThanDebugLoc">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3205 */
  export declare function GetInstructionParent(Inst: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3215 */
  export declare function GetNextInstruction(Inst: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3223 */
  export declare function GetPreviousInstruction(Inst: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3233 */
  export declare function InstructionRemoveFromParent(Inst: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3243 */
  export declare function InstructionEraseFromParent(Inst: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3253 */
  export declare function DeleteInstruction(Inst: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3260 */
  export declare function GetInstructionOpcode(Inst: LLVM.ValueRef): LLVM.Opcode;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3270 */
  export declare function GetICmpPredicate(Inst: LLVM.ValueRef): LLVM.IntPredicate;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3280 */
  export declare function GetFCmpPredicate(Inst: LLVM.ValueRef): LLVM.RealPredicate;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3290 */
  export declare function InstructionClone(Inst: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3299 */
  export declare function IsATerminatorInst(Inst: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3321 */
  export declare function GetNumArgOperands(Instr: LLVM.ValueRef): Opaque<number, "LLVMGetNumArgOperands">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3332 */
  export declare function SetInstructionCallConv(Instr: LLVM.ValueRef, CC: Opaque<number, "CC">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3342 */
  export declare function GetInstructionCallConv(Instr: LLVM.ValueRef): Opaque<number, "LLVMGetInstructionCallConv">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3344 */
  export declare function SetInstrParamAlignment(Instr: LLVM.ValueRef, Idx: LLVM.AttributeIndex, Align: Opaque<number, "Align">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3347 */
  export declare function AddCallSiteAttribute(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex, A: LLVM.AttributeRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3349 */
  export declare function GetCallSiteAttributeCount(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex): Opaque<number, "LLVMGetCallSiteAttributeCount">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3350 */
  export declare function GetCallSiteAttributes(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex, Attrs: Pointer<"Attrs">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3352 */
  export declare function GetCallSiteEnumAttribute(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex, KindID: Opaque<number, "KindID">): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3355 */
  export declare function GetCallSiteStringAttribute(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex, K: Pointer<"K">, KLen: Opaque<number, "KLen">): LLVM.AttributeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3358 */
  export declare function RemoveCallSiteEnumAttribute(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex, KindID: Opaque<number, "KindID">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3360 */
  export declare function RemoveCallSiteStringAttribute(C: LLVM.ValueRef, Idx: LLVM.AttributeIndex, K: Pointer<"K">, KLen: Opaque<number, "KLen">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3368 */
  export declare function GetCalledFunctionType(C: LLVM.ValueRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3379 */
  export declare function GetCalledValue(Instr: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3388 */
  export declare function IsTailCall(CallInst: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3397 */
  export declare function SetTailCall(CallInst: LLVM.ValueRef, IsTailCall: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3406 */
  export declare function GetNormalDest(InvokeInst: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3418 */
  export declare function GetUnwindDest(InvokeInst: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3427 */
  export declare function SetNormalDest(InvokeInst: LLVM.ValueRef, B: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3439 */
  export declare function SetUnwindDest(InvokeInst: LLVM.ValueRef, B: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3459 */
  export declare function GetNumSuccessors(Term: LLVM.ValueRef): Opaque<number, "LLVMGetNumSuccessors">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3466 */
  export declare function GetSuccessor(Term: LLVM.ValueRef, i: Opaque<number, "i">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3473 */
  export declare function SetSuccessor(Term: LLVM.ValueRef, i: Opaque<number, "i">, block: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3482 */
  export declare function IsConditional(Branch: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3491 */
  export declare function GetCondition(Branch: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3500 */
  export declare function SetCondition(Branch: LLVM.ValueRef, Cond: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3509 */
  export declare function GetSwitchDefaultDest(SwitchInstr: LLVM.ValueRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3527 */
  export declare function GetAllocatedType(Alloca: LLVM.ValueRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3545 */
  export declare function IsInBounds(GEP: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3550 */
  export declare function SetIsInBounds(GEP: LLVM.ValueRef, InBounds: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3555 */
  export declare function GetGEPSourceElementType(GEP: LLVM.ValueRef): LLVM.TypeRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3573 */
  export declare function AddIncoming(PhiNode: LLVM.ValueRef, IncomingValues: Pointer<"IncomingValues">, IncomingBlocks: Pointer<"IncomingBlocks">, Count: Opaque<number, "Count">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3579 */
  export declare function CountIncoming(PhiNode: LLVM.ValueRef): Opaque<number, "LLVMCountIncoming">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3584 */
  export declare function GetIncomingValue(PhiNode: LLVM.ValueRef, Index: Opaque<number, "Index">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3589 */
  export declare function GetIncomingBlock(PhiNode: LLVM.ValueRef, Index: Opaque<number, "Index">): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3609 */
  export declare function GetNumIndices(Inst: LLVM.ValueRef): Opaque<number, "LLVMGetNumIndices">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3614 */
  export declare function GetIndices(Inst: LLVM.ValueRef): Pointer<"LLVMGetIndices">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3637 */
  export declare function CreateBuilderInContext(C: LLVM.ContextRef): LLVM.BuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3638 */
  export declare function CreateBuilder(): LLVM.BuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3639 */
  export declare function PositionBuilder(Builder: LLVM.BuilderRef, Block: LLVM.BasicBlockRef, Instr: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3641 */
  export declare function PositionBuilderBefore(Builder: LLVM.BuilderRef, Instr: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3642 */
  export declare function PositionBuilderAtEnd(Builder: LLVM.BuilderRef, Block: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3643 */
  export declare function GetInsertBlock(Builder: LLVM.BuilderRef): LLVM.BasicBlockRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3644 */
  export declare function ClearInsertionPosition(Builder: LLVM.BuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3645 */
  export declare function InsertIntoBuilder(Builder: LLVM.BuilderRef, Instr: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3646 */
  export declare function InsertIntoBuilderWithName(Builder: LLVM.BuilderRef, Instr: LLVM.ValueRef, Name: Pointer<"Name">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3648 */
  export declare function DisposeBuilder(Builder: LLVM.BuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3657 */
  export declare function GetCurrentDebugLocation2(Builder: LLVM.BuilderRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3666 */
  export declare function SetCurrentDebugLocation2(Builder: LLVM.BuilderRef, Loc: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3678 */
  export declare function SetInstDebugLocation(Builder: LLVM.BuilderRef, Inst: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3685 */
  export declare function AddMetadataToInst(Builder: LLVM.BuilderRef, Inst: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3692 */
  export declare function BuilderGetDefaultFPMathTag(Builder: LLVM.BuilderRef): LLVM.MetadataRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3701 */
  export declare function BuilderSetDefaultFPMathTag(Builder: LLVM.BuilderRef, FPMathTag: LLVM.MetadataRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3708 */
  export declare function SetCurrentDebugLocation(Builder: LLVM.BuilderRef, L: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3713 */
  export declare function GetCurrentDebugLocation(Builder: LLVM.BuilderRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3716 */
  export declare function BuildRetVoid(_0: LLVM.BuilderRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3717 */
  export declare function BuildRet(_0: LLVM.BuilderRef, V: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3718 */
  export declare function BuildAggregateRet(_0: LLVM.BuilderRef, RetVals: Pointer<"RetVals">, N: Opaque<number, "N">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3720 */
  export declare function BuildBr(_0: LLVM.BuilderRef, Dest: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3721 */
  export declare function BuildCondBr(_0: LLVM.BuilderRef, If: LLVM.ValueRef, Then: LLVM.BasicBlockRef, Else: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3723 */
  export declare function BuildSwitch(_0: LLVM.BuilderRef, V: LLVM.ValueRef, Else: LLVM.BasicBlockRef, NumCases: Opaque<number, "NumCases">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3725 */
  export declare function BuildIndirectBr(B: LLVM.BuilderRef, Addr: LLVM.ValueRef, NumDests: Opaque<number, "NumDests">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3727 */
  export declare function BuildInvoke(_0: LLVM.BuilderRef, Fn: LLVM.ValueRef, Args: Pointer<"Args">, NumArgs: Opaque<number, "NumArgs">, Then: LLVM.BasicBlockRef, Catch: LLVM.BasicBlockRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3733 */
  export declare function BuildInvoke2(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, Fn: LLVM.ValueRef, Args: Pointer<"Args">, NumArgs: Opaque<number, "NumArgs">, Then: LLVM.BasicBlockRef, Catch: LLVM.BasicBlockRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3737 */
  export declare function BuildUnreachable(_0: LLVM.BuilderRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3740 */
  export declare function BuildResume(B: LLVM.BuilderRef, Exn: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3741 */
  export declare function BuildLandingPad(B: LLVM.BuilderRef, Ty: LLVM.TypeRef, PersFn: LLVM.ValueRef, NumClauses: Opaque<number, "NumClauses">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3744 */
  export declare function BuildCleanupRet(B: LLVM.BuilderRef, CatchPad: LLVM.ValueRef, BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3746 */
  export declare function BuildCatchRet(B: LLVM.BuilderRef, CatchPad: LLVM.ValueRef, BB: LLVM.BasicBlockRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3748 */
  export declare function BuildCatchPad(B: LLVM.BuilderRef, ParentPad: LLVM.ValueRef, Args: Pointer<"Args">, NumArgs: Opaque<number, "NumArgs">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3751 */
  export declare function BuildCleanupPad(B: LLVM.BuilderRef, ParentPad: LLVM.ValueRef, Args: Pointer<"Args">, NumArgs: Opaque<number, "NumArgs">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3754 */
  export declare function BuildCatchSwitch(B: LLVM.BuilderRef, ParentPad: LLVM.ValueRef, UnwindBB: LLVM.BasicBlockRef, NumHandlers: Opaque<number, "NumHandlers">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3759 */
  export declare function AddCase(Switch: LLVM.ValueRef, OnVal: LLVM.ValueRef, Dest: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3763 */
  export declare function AddDestination(IndirectBr: LLVM.ValueRef, Dest: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3766 */
  export declare function GetNumClauses(LandingPad: LLVM.ValueRef): Opaque<number, "LLVMGetNumClauses">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3769 */
  export declare function GetClause(LandingPad: LLVM.ValueRef, Idx: Opaque<number, "Idx">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3772 */
  export declare function AddClause(LandingPad: LLVM.ValueRef, ClauseVal: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3775 */
  export declare function IsCleanup(LandingPad: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3778 */
  export declare function SetCleanup(LandingPad: LLVM.ValueRef, Val: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3781 */
  export declare function AddHandler(CatchSwitch: LLVM.ValueRef, Dest: LLVM.BasicBlockRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3784 */
  export declare function GetNumHandlers(CatchSwitch: LLVM.ValueRef): Opaque<number, "LLVMGetNumHandlers">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3797 */
  export declare function GetHandlers(CatchSwitch: LLVM.ValueRef, Handlers: Pointer<"Handlers">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3802 */
  export declare function GetArgOperand(Funclet: LLVM.ValueRef, i: Opaque<number, "i">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3805 */
  export declare function SetArgOperand(Funclet: LLVM.ValueRef, i: Opaque<number, "i">, value: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3814 */
  export declare function GetParentCatchSwitch(CatchPad: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3823 */
  export declare function SetParentCatchSwitch(CatchPad: LLVM.ValueRef, CatchSwitch: LLVM.ValueRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3826 */
  export declare function BuildAdd(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3828 */
  export declare function BuildNSWAdd(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3830 */
  export declare function BuildNUWAdd(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3832 */
  export declare function BuildFAdd(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3834 */
  export declare function BuildSub(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3836 */
  export declare function BuildNSWSub(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3838 */
  export declare function BuildNUWSub(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3840 */
  export declare function BuildFSub(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3842 */
  export declare function BuildMul(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3844 */
  export declare function BuildNSWMul(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3846 */
  export declare function BuildNUWMul(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3848 */
  export declare function BuildFMul(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3850 */
  export declare function BuildUDiv(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3852 */
  export declare function BuildExactUDiv(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3854 */
  export declare function BuildSDiv(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3856 */
  export declare function BuildExactSDiv(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3858 */
  export declare function BuildFDiv(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3860 */
  export declare function BuildURem(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3862 */
  export declare function BuildSRem(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3864 */
  export declare function BuildFRem(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3866 */
  export declare function BuildShl(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3868 */
  export declare function BuildLShr(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3870 */
  export declare function BuildAShr(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3872 */
  export declare function BuildAnd(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3874 */
  export declare function BuildOr(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3876 */
  export declare function BuildXor(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3878 */
  export declare function BuildBinOp(B: LLVM.BuilderRef, Op: LLVM.Opcode, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3881 */
  export declare function BuildNeg(_0: LLVM.BuilderRef, V: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3882 */
  export declare function BuildNSWNeg(B: LLVM.BuilderRef, V: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3884 */
  export declare function BuildNUWNeg(B: LLVM.BuilderRef, V: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3886 */
  export declare function BuildFNeg(_0: LLVM.BuilderRef, V: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3887 */
  export declare function BuildNot(_0: LLVM.BuilderRef, V: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3890 */
  export declare function BuildMalloc(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3891 */
  export declare function BuildArrayMalloc(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, Val: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3900 */
  export declare function BuildMemSet(B: LLVM.BuilderRef, Ptr: LLVM.ValueRef, Val: LLVM.ValueRef, Len: LLVM.ValueRef, Align: Opaque<number, "Align">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3908 */
  export declare function BuildMemCpy(B: LLVM.BuilderRef, Dst: LLVM.ValueRef, DstAlign: Opaque<number, "DstAlign">, Src: LLVM.ValueRef, SrcAlign: Opaque<number, "SrcAlign">, Size: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3917 */
  export declare function BuildMemMove(B: LLVM.BuilderRef, Dst: LLVM.ValueRef, DstAlign: Opaque<number, "DstAlign">, Src: LLVM.ValueRef, SrcAlign: Opaque<number, "SrcAlign">, Size: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3922 */
  export declare function BuildAlloca(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3923 */
  export declare function BuildArrayAlloca(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, Val: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3925 */
  export declare function BuildFree(_0: LLVM.BuilderRef, PointerVal: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3926 */
  export declare function BuildLoad(_0: LLVM.BuilderRef, PointerVal: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3930 */
  export declare function BuildLoad2(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, PointerVal: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3932 */
  export declare function BuildStore(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, Ptr: LLVM.ValueRef): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3933 */
  export declare function BuildGEP(B: LLVM.BuilderRef, Pointer: LLVM.ValueRef, Indices: Pointer<"Indices">, NumIndices: Opaque<number, "NumIndices">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3938 */
  export declare function BuildInBoundsGEP(B: LLVM.BuilderRef, Pointer: LLVM.ValueRef, Indices: Pointer<"Indices">, NumIndices: Opaque<number, "NumIndices">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3943 */
  export declare function BuildStructGEP(B: LLVM.BuilderRef, Pointer: LLVM.ValueRef, Idx: Opaque<number, "Idx">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3947 */
  export declare function BuildGEP2(B: LLVM.BuilderRef, Ty: LLVM.TypeRef, Pointer: LLVM.ValueRef, Indices: Pointer<"Indices">, NumIndices: Opaque<number, "NumIndices">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3950 */
  export declare function BuildInBoundsGEP2(B: LLVM.BuilderRef, Ty: LLVM.TypeRef, Pointer: LLVM.ValueRef, Indices: Pointer<"Indices">, NumIndices: Opaque<number, "NumIndices">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3953 */
  export declare function BuildStructGEP2(B: LLVM.BuilderRef, Ty: LLVM.TypeRef, Pointer: LLVM.ValueRef, Idx: Opaque<number, "Idx">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3956 */
  export declare function BuildGlobalString(B: LLVM.BuilderRef, Str: Pointer<"Str">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3958 */
  export declare function BuildGlobalStringPtr(B: LLVM.BuilderRef, Str: Pointer<"Str">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3960 */
  export declare function GetVolatile(MemoryAccessInst: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3961 */
  export declare function SetVolatile(MemoryAccessInst: LLVM.ValueRef, IsVolatile: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3962 */
  export declare function GetWeak(CmpXchgInst: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3963 */
  export declare function SetWeak(CmpXchgInst: LLVM.ValueRef, IsWeak: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3964 */
  export declare function GetOrdering(MemoryAccessInst: LLVM.ValueRef): LLVM.AtomicOrdering;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3965 */
  export declare function SetOrdering(MemoryAccessInst: LLVM.ValueRef, Ordering: LLVM.AtomicOrdering): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3966 */
  export declare function GetAtomicRMWBinOp(AtomicRMWInst: LLVM.ValueRef): LLVM.AtomicRMWBinOp;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3967 */
  export declare function SetAtomicRMWBinOp(AtomicRMWInst: LLVM.ValueRef, BinOp: LLVM.AtomicRMWBinOp): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3970 */
  export declare function BuildTrunc(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3972 */
  export declare function BuildZExt(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3974 */
  export declare function BuildSExt(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3976 */
  export declare function BuildFPToUI(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3978 */
  export declare function BuildFPToSI(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3980 */
  export declare function BuildUIToFP(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3982 */
  export declare function BuildSIToFP(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3984 */
  export declare function BuildFPTrunc(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3986 */
  export declare function BuildFPExt(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3988 */
  export declare function BuildPtrToInt(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3990 */
  export declare function BuildIntToPtr(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3992 */
  export declare function BuildBitCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3994 */
  export declare function BuildAddrSpaceCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3996 */
  export declare function BuildZExtOrBitCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L3998 */
  export declare function BuildSExtOrBitCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4000 */
  export declare function BuildTruncOrBitCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4002 */
  export declare function BuildCast(B: LLVM.BuilderRef, Op: LLVM.Opcode, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4004 */
  export declare function BuildPointerCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4006 */
  export declare function BuildIntCast2(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, IsSigned: LLVM.Bool, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4009 */
  export declare function BuildFPCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4013 */
  export declare function BuildIntCast(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, DestTy: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4016 */
  export declare function GetCastOpcode(Src: LLVM.ValueRef, SrcIsSigned: LLVM.Bool, DestTy: LLVM.TypeRef, DestIsSigned: LLVM.Bool): LLVM.Opcode;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4020 */
  export declare function BuildICmp(_0: LLVM.BuilderRef, Op: LLVM.IntPredicate, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4023 */
  export declare function BuildFCmp(_0: LLVM.BuilderRef, Op: LLVM.RealPredicate, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4028 */
  export declare function BuildPhi(_0: LLVM.BuilderRef, Ty: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4029 */
  export declare function BuildCall(_0: LLVM.BuilderRef, Fn: LLVM.ValueRef, Args: Pointer<"Args">, NumArgs: Opaque<number, "NumArgs">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4034 */
  export declare function BuildCall2(_0: LLVM.BuilderRef, _1: LLVM.TypeRef, Fn: LLVM.ValueRef, Args: Pointer<"Args">, NumArgs: Opaque<number, "NumArgs">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4037 */
  export declare function BuildSelect(_0: LLVM.BuilderRef, If: LLVM.ValueRef, Then: LLVM.ValueRef, Else: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4040 */
  export declare function BuildVAArg(_0: LLVM.BuilderRef, List: LLVM.ValueRef, Ty: LLVM.TypeRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4042 */
  export declare function BuildExtractElement(_0: LLVM.BuilderRef, VecVal: LLVM.ValueRef, Index: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4044 */
  export declare function BuildInsertElement(_0: LLVM.BuilderRef, VecVal: LLVM.ValueRef, EltVal: LLVM.ValueRef, Index: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4047 */
  export declare function BuildShuffleVector(_0: LLVM.BuilderRef, V1: LLVM.ValueRef, V2: LLVM.ValueRef, Mask: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4050 */
  export declare function BuildExtractValue(_0: LLVM.BuilderRef, AggVal: LLVM.ValueRef, Index: Opaque<number, "Index">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4052 */
  export declare function BuildInsertValue(_0: LLVM.BuilderRef, AggVal: LLVM.ValueRef, EltVal: LLVM.ValueRef, Index: Opaque<number, "Index">, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4055 */
  export declare function BuildFreeze(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4058 */
  export declare function BuildIsNull(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4060 */
  export declare function BuildIsNotNull(_0: LLVM.BuilderRef, Val: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4062 */
  export declare function BuildPtrDiff(_0: LLVM.BuilderRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4066 */
  export declare function BuildPtrDiff2(_0: LLVM.BuilderRef, ElemTy: LLVM.TypeRef, LHS: LLVM.ValueRef, RHS: LLVM.ValueRef, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4069 */
  export declare function BuildFence(B: LLVM.BuilderRef, ordering: LLVM.AtomicOrdering, singleThread: LLVM.Bool, Name: Pointer<"Name">): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4071 */
  export declare function BuildAtomicRMW(B: LLVM.BuilderRef, op: LLVM.AtomicRMWBinOp, PTR: LLVM.ValueRef, Val: LLVM.ValueRef, ordering: LLVM.AtomicOrdering, singleThread: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4075 */
  export declare function BuildAtomicCmpXchg(B: LLVM.BuilderRef, Ptr: LLVM.ValueRef, Cmp: LLVM.ValueRef, New: LLVM.ValueRef, SuccessOrdering: LLVM.AtomicOrdering, FailureOrdering: LLVM.AtomicOrdering, SingleThread: LLVM.Bool): LLVM.ValueRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4084 */
  export declare function GetNumMaskElements(ShuffleVectorInst: LLVM.ValueRef): Opaque<number, "LLVMGetNumMaskElements">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4090 */
  export declare function GetUndefMaskElem(): Opaque<number, "LLVMGetUndefMaskElem">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4099 */
  export declare function GetMaskValue(ShuffleVectorInst: LLVM.ValueRef, Elt: Opaque<number, "Elt">): Opaque<number, "LLVMGetMaskValue">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4101 */
  export declare function IsAtomicSingleThread(AtomicInst: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4102 */
  export declare function SetAtomicSingleThread(AtomicInst: LLVM.ValueRef, SingleThread: LLVM.Bool): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4104 */
  export declare function GetCmpXchgSuccessOrdering(CmpXchgInst: LLVM.ValueRef): LLVM.AtomicOrdering;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4105 */
  export declare function SetCmpXchgSuccessOrdering(CmpXchgInst: LLVM.ValueRef, Ordering: LLVM.AtomicOrdering): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4107 */
  export declare function GetCmpXchgFailureOrdering(CmpXchgInst: LLVM.ValueRef): LLVM.AtomicOrdering;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4108 */
  export declare function SetCmpXchgFailureOrdering(CmpXchgInst: LLVM.ValueRef, Ordering: LLVM.AtomicOrdering): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4126 */
  export declare function CreateModuleProviderForExistingModule(M: LLVM.ModuleRef): LLVM.ModuleProviderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4131 */
  export declare function DisposeModuleProvider(M: LLVM.ModuleProviderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4143 */
  export declare function CreateMemoryBufferWithContentsOfFile(Path: Pointer<"Path">, OutMemBuf: Pointer<"OutMemBuf">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4146 */
  export declare function CreateMemoryBufferWithSTDIN(OutMemBuf: Pointer<"OutMemBuf">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4148 */
  export declare function CreateMemoryBufferWithMemoryRange(InputData: Pointer<"InputData">, InputDataLength: Opaque<number, "InputDataLength">, BufferName: Pointer<"BufferName">, RequiresNullTerminator: LLVM.Bool): LLVM.MemoryBufferRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4152 */
  export declare function CreateMemoryBufferWithMemoryRangeCopy(InputData: Pointer<"InputData">, InputDataLength: Opaque<number, "InputDataLength">, BufferName: Pointer<"BufferName">): LLVM.MemoryBufferRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4155 */
  export declare function GetBufferStart(MemBuf: LLVM.MemoryBufferRef): Pointer<"LLVMGetBufferStart">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4156 */
  export declare function GetBufferSize(MemBuf: LLVM.MemoryBufferRef): Opaque<number, "LLVMGetBufferSize">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4157 */
  export declare function DisposeMemoryBuffer(MemBuf: LLVM.MemoryBufferRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4172 */
  export declare function GetGlobalPassRegistry(): LLVM.PassRegistryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4188 */
  export declare function CreatePassManager(): LLVM.PassManagerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4194 */
  export declare function CreateFunctionPassManagerForModule(M: LLVM.ModuleRef): LLVM.PassManagerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4197 */
  export declare function CreateFunctionPassManager(MP: LLVM.ModuleProviderRef): LLVM.PassManagerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4203 */
  export declare function RunPassManager(PM: LLVM.PassManagerRef, M: LLVM.ModuleRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4208 */
  export declare function InitializeFunctionPassManager(FPM: LLVM.PassManagerRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4214 */
  export declare function RunFunctionPassManager(FPM: LLVM.PassManagerRef, F: LLVM.ValueRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4219 */
  export declare function FinalizeFunctionPassManager(FPM: LLVM.PassManagerRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4224 */
  export declare function DisposePassManager(PM: LLVM.PassManagerRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4241 */
  export declare function StartMultithreaded(): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4245 */
  export declare function StopMultithreaded(): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Core.h#L4249 */
  export declare function IsMultithreaded(): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L76 */
  export declare function CreateBinary(MemBuf: LLVM.MemoryBufferRef, Context: LLVM.ContextRef, ErrorMessage: Pointer<"ErrorMessage">): LLVM.BinaryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L86 */
  export declare function DisposeBinary(BR: LLVM.BinaryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L97 */
  export declare function BinaryCopyMemoryBuffer(BR: LLVM.BinaryRef): LLVM.MemoryBufferRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L104 */
  export declare function BinaryGetType(BR: LLVM.BinaryRef): LLVM.BinaryType;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L117 */
  export declare function MachOUniversalBinaryCopyObjectForArch(BR: LLVM.BinaryRef, Arch: Pointer<"Arch">, ArchLen: Opaque<number, "ArchLen">, ErrorMessage: Pointer<"ErrorMessage">): LLVM.BinaryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L133 */
  export declare function ObjectFileCopySectionIterator(BR: LLVM.BinaryRef): LLVM.SectionIteratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L140 */
  export declare function ObjectFileIsSectionIteratorAtEnd(BR: LLVM.BinaryRef, SI: LLVM.SectionIteratorRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L154 */
  export declare function ObjectFileCopySymbolIterator(BR: LLVM.BinaryRef): LLVM.SymbolIteratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L161 */
  export declare function ObjectFileIsSymbolIteratorAtEnd(BR: LLVM.BinaryRef, SI: LLVM.SymbolIteratorRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L164 */
  export declare function DisposeSectionIterator(SI: LLVM.SectionIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L166 */
  export declare function MoveToNextSection(SI: LLVM.SectionIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L167 */
  export declare function MoveToContainingSection(Sect: LLVM.SectionIteratorRef, Sym: LLVM.SymbolIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L171 */
  export declare function DisposeSymbolIterator(SI: LLVM.SymbolIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L172 */
  export declare function MoveToNextSymbol(SI: LLVM.SymbolIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L175 */
  export declare function GetSectionName(SI: LLVM.SectionIteratorRef): Pointer<"LLVMGetSectionName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L176 */
  export declare function GetSectionSize(SI: LLVM.SectionIteratorRef): Opaque<bigint, "LLVMGetSectionSize">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L177 */
  export declare function GetSectionContents(SI: LLVM.SectionIteratorRef): Pointer<"LLVMGetSectionContents">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L178 */
  export declare function GetSectionAddress(SI: LLVM.SectionIteratorRef): Opaque<bigint, "LLVMGetSectionAddress">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L179 */
  export declare function GetSectionContainsSymbol(SI: LLVM.SectionIteratorRef, Sym: LLVM.SymbolIteratorRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L183 */
  export declare function GetRelocations(Section: LLVM.SectionIteratorRef): LLVM.RelocationIteratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L184 */
  export declare function DisposeRelocationIterator(RI: LLVM.RelocationIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L185 */
  export declare function IsRelocationIteratorAtEnd(Section: LLVM.SectionIteratorRef, RI: LLVM.RelocationIteratorRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L187 */
  export declare function MoveToNextRelocation(RI: LLVM.RelocationIteratorRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L191 */
  export declare function GetSymbolName(SI: LLVM.SymbolIteratorRef): Pointer<"LLVMGetSymbolName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L192 */
  export declare function GetSymbolAddress(SI: LLVM.SymbolIteratorRef): Opaque<bigint, "LLVMGetSymbolAddress">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L193 */
  export declare function GetSymbolSize(SI: LLVM.SymbolIteratorRef): Opaque<bigint, "LLVMGetSymbolSize">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L196 */
  export declare function GetRelocationOffset(RI: LLVM.RelocationIteratorRef): Opaque<bigint, "LLVMGetRelocationOffset">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L197 */
  export declare function GetRelocationSymbol(RI: LLVM.RelocationIteratorRef): LLVM.SymbolIteratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L198 */
  export declare function GetRelocationType(RI: LLVM.RelocationIteratorRef): Opaque<bigint, "LLVMGetRelocationType">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L201 */
  export declare function GetRelocationTypeName(RI: LLVM.RelocationIteratorRef): Pointer<"LLVMGetRelocationTypeName">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L202 */
  export declare function GetRelocationValueString(RI: LLVM.RelocationIteratorRef): Pointer<"LLVMGetRelocationValueString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L208 */
  export declare function CreateObjectFile(MemBuf: LLVM.MemoryBufferRef): LLVM.ObjectFileRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L211 */
  export declare function DisposeObjectFile(ObjectFile: LLVM.ObjectFileRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L214 */
  export declare function GetSections(ObjectFile: LLVM.ObjectFileRef): LLVM.SectionIteratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L217 */
  export declare function IsSectionIteratorAtEnd(ObjectFile: LLVM.ObjectFileRef, SI: LLVM.SectionIteratorRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L221 */
  export declare function GetSymbols(ObjectFile: LLVM.ObjectFileRef): LLVM.SymbolIteratorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Object.h#L224 */
  export declare function IsSymbolIteratorAtEnd(ObjectFile: LLVM.ObjectFileRef, SI: LLVM.SymbolIteratorRef): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitWriter.h#L37 */
  export declare function WriteBitcodeToFile(M: LLVM.ModuleRef, Path: Pointer<"Path">): Opaque<number, "LLVMWriteBitcodeToFile">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitWriter.h#L40 */
  export declare function WriteBitcodeToFD(M: LLVM.ModuleRef, FD: Opaque<number, "FD">, ShouldClose: Opaque<number, "ShouldClose">, Unbuffered: Opaque<number, "Unbuffered">): Opaque<number, "LLVMWriteBitcodeToFD">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitWriter.h#L45 */
  export declare function WriteBitcodeToFileHandle(M: LLVM.ModuleRef, Handle: Opaque<number, "Handle">): Opaque<number, "LLVMWriteBitcodeToFileHandle">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitWriter.h#L48 */
  export declare function WriteBitcodeToMemoryBuffer(M: LLVM.ModuleRef): LLVM.MemoryBufferRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L74 */
  export declare function OrcCreateLLJITBuilder(): LLVM.OrcLLJITBuilderRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L81 */
  export declare function OrcDisposeLLJITBuilder(Builder: LLVM.OrcLLJITBuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L92 */
  export declare function OrcLLJITBuilderSetJITTargetMachineBuilder(Builder: LLVM.OrcLLJITBuilderRef, JTMB: LLVM.OrcJITTargetMachineBuilderRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L98 */
  export declare function OrcLLJITBuilderSetObjectLinkingLayerCreator(Builder: LLVM.OrcLLJITBuilderRef, F: LLVM.OrcLLJITBuilderObjectLinkingLayerCreatorFunction, Ctx: Pointer<"Ctx">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L116 */
  export declare function OrcCreateLLJIT(Result: Pointer<"Result">, Builder: LLVM.OrcLLJITBuilderRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L122 */
  export declare function OrcDisposeLLJIT(J: LLVM.OrcLLJITRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L130 */
  export declare function OrcLLJITGetExecutionSession(J: LLVM.OrcLLJITRef): LLVM.OrcExecutionSessionRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L138 */
  export declare function OrcLLJITGetMainJITDylib(J: LLVM.OrcLLJITRef): LLVM.OrcJITDylibRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L144 */
  export declare function OrcLLJITGetTripleString(J: LLVM.OrcLLJITRef): Pointer<"LLVMOrcLLJITGetTripleString">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L149 */
  export declare function OrcLLJITGetGlobalPrefix(J: LLVM.OrcLLJITRef): Opaque<number, "LLVMOrcLLJITGetGlobalPrefix">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L159 */
  export declare function OrcLLJITMangleAndIntern(J: LLVM.OrcLLJITRef, UnmangledName: Pointer<"UnmangledName">): LLVM.OrcSymbolStringPoolEntryRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L170 */
  export declare function OrcLLJITAddObjectFile(J: LLVM.OrcLLJITRef, JD: LLVM.OrcJITDylibRef, ObjBuffer: LLVM.MemoryBufferRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L182 */
  export declare function OrcLLJITAddObjectFileWithRT(J: LLVM.OrcLLJITRef, RT: LLVM.OrcResourceTrackerRef, ObjBuffer: LLVM.MemoryBufferRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L195 */
  export declare function OrcLLJITAddLLVMIRModule(J: LLVM.OrcLLJITRef, JD: LLVM.OrcJITDylibRef, TSM: LLVM.OrcThreadSafeModuleRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L208 */
  export declare function OrcLLJITAddLLVMIRModuleWithRT(J: LLVM.OrcLLJITRef, JD: LLVM.OrcResourceTrackerRef, TSM: LLVM.OrcThreadSafeModuleRef): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L217 */
  export declare function OrcLLJITLookup(J: LLVM.OrcLLJITRef, Result: Pointer<"Result">, Name: Pointer<"Name">): LLVM.ErrorRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L224 */
  export declare function OrcLLJITGetObjLinkingLayer(J: LLVM.OrcLLJITRef): LLVM.OrcObjectLayerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L230 */
  export declare function OrcLLJITGetObjTransformLayer(J: LLVM.OrcLLJITRef): LLVM.OrcObjectTransformLayerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L235 */
  export declare function OrcLLJITGetIRTransformLayer(J: LLVM.OrcLLJITRef): LLVM.OrcIRTransformLayerRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/LLJIT.h#L243 */
  export declare function OrcLLJITGetDataLayoutStr(J: LLVM.OrcLLJITRef): Pointer<"LLVMOrcLLJITGetDataLayoutStr">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Support.h#L35 */
  export declare function LoadLibraryPermanently(Filename: Pointer<"Filename">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Support.h#L45 */
  export declare function ParseCommandLineOptions(argc: Opaque<number, "argc">, argv: Pointer<"argv">, Overview: Pointer<"Overview">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Support.h#L55 */
  export declare function SearchForAddressOfSymbol(symbolName: Pointer<"symbolName">): Pointer<"LLVMSearchForAddressOfSymbol">;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Support.h#L64 */
  export declare function AddSymbol(symbolName: Pointer<"symbolName">, symbolValue: Pointer<"symbolValue">): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L34 */
  export declare function InitializeTransformUtils(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L35 */
  export declare function InitializeScalarOpts(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L36 */
  export declare function InitializeObjCARCOpts(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L37 */
  export declare function InitializeVectorization(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L38 */
  export declare function InitializeInstCombine(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L39 */
  export declare function InitializeAggressiveInstCombiner(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L40 */
  export declare function InitializeIPO(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L41 */
  export declare function InitializeInstrumentation(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L42 */
  export declare function InitializeAnalysis(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L43 */
  export declare function InitializeIPA(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L44 */
  export declare function InitializeCodeGen(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Initialization.h#L45 */
  export declare function InitializeTarget(R: LLVM.PassRegistryRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L39 */
  export declare function ParseBitcode(MemBuf: LLVM.MemoryBufferRef, OutModule: Pointer<"OutModule">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L44 */
  export declare function ParseBitcode2(MemBuf: LLVM.MemoryBufferRef, OutModule: Pointer<"OutModule">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L48 */
  export declare function ParseBitcodeInContext(ContextRef: LLVM.ContextRef, MemBuf: LLVM.MemoryBufferRef, OutModule: Pointer<"OutModule">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L52 */
  export declare function ParseBitcodeInContext2(ContextRef: LLVM.ContextRef, MemBuf: LLVM.MemoryBufferRef, OutModule: Pointer<"OutModule">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L60 */
  export declare function GetBitcodeModuleInContext(ContextRef: LLVM.ContextRef, MemBuf: LLVM.MemoryBufferRef, OutM: Pointer<"OutM">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L71 */
  export declare function GetBitcodeModuleInContext2(ContextRef: LLVM.ContextRef, MemBuf: LLVM.MemoryBufferRef, OutM: Pointer<"OutM">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L76 */
  export declare function GetBitcodeModule(MemBuf: LLVM.MemoryBufferRef, OutM: Pointer<"OutM">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/BitReader.h#L79 */
  export declare function GetBitcodeModule2(MemBuf: LLVM.MemoryBufferRef, OutM: Pointer<"OutM">): LLVM.Bool;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Comdat.h#L46 */
  export declare function GetOrInsertComdat(M: LLVM.ModuleRef, Name: Pointer<"Name">): LLVM.ComdatRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Comdat.h#L53 */
  export declare function GetComdat(V: LLVM.ValueRef): LLVM.ComdatRef;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Comdat.h#L60 */
  export declare function SetComdat(V: LLVM.ValueRef, C: LLVM.ComdatRef): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Comdat.h#L67 */
  export declare function GetComdatSelectionKind(C: LLVM.ComdatRef): LLVM.ComdatSelectionKind;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/Comdat.h#L74 */
  export declare function SetComdatSelectionKind(C: LLVM.ComdatRef, Kind: LLVM.ComdatSelectionKind): void;

  /** https://github.com/llvm/llvm-project/blob/315072/llvm/include/llvm-c/IRReader.h#L38 */
  export declare function ParseIRInContext(ContextRef: LLVM.ContextRef, MemBuf: LLVM.MemoryBufferRef, OutM: Pointer<"OutM">, OutMessage: Pointer<"OutMessage">): LLVM.Bool;

  export declare function close(): void;
}
