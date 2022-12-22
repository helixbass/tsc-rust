use itertools::Itertools;
use lazy_static::lazy_static;
use pretty_assertions::assert_str_eq;
use regex::{Captures, Regex};
use rstest::rstest;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::rc::Rc;

use typescript_rust::{
    create_compiler_host_worker, create_program, format_diagnostics,
    format_diagnostics_with_color_and_context, get_emit_script_target, get_pre_emit_diagnostics,
    get_sys, option_declarations, parse_custom_type_option, parse_list_type_option,
    read_file_and_strip_leading_byte_order_mark, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder, CompilerOptionsValue,
    CreateProgramOptions, Diagnostic, FormatDiagnosticsHost, NewLineKind, Node,
};

#[rstest]
#[case("2dArrays.ts")]
#[case("APISample_Watch.ts")] // NOT RUNNABLE
#[case("APISample_WatchWithDefaults.ts")] // NOT RUNNABLE
#[case("APISample_WatchWithOwnWatchHost.ts")] // NOT RUNNABLE
#[case("APISample_compile.ts")] // NOT RUNNABLE
#[case("APISample_jsdoc.ts")] // NOT RUNNABLE
#[case("APISample_linter.ts")] // NOT RUNNABLE
#[case("APISample_parseConfig.ts")] // NOT RUNNABLE
#[case("APISample_transform.ts")] // NOT RUNNABLE
#[case("APISample_watcher.ts")] // NOT RUNNABLE
#[case("ArrowFunctionExpression1.ts")]
#[case("ClassDeclaration10.ts")]
#[case("ClassDeclaration11.ts")]
#[case("ClassDeclaration13.ts")]
#[case("ClassDeclaration14.ts")]
#[case("ClassDeclaration15.ts")]
#[case("ClassDeclaration21.ts")]
#[case("ClassDeclaration22.ts")]
#[case("ClassDeclaration24.ts")]
#[case("ClassDeclaration25.ts")]
#[case("ClassDeclaration26.ts")]
#[case("ClassDeclaration8.ts")]
#[case("ClassDeclaration9.ts")]
#[case("ClassDeclarationWithInvalidConstOnPropertyDeclaration.ts")]
#[case("ClassDeclarationWithInvalidConstOnPropertyDeclaration2.ts")]
#[case("DeclarationErrorsNoEmitOnError.ts")]
#[case("ExportAssignment7.ts")]
#[case("ExportAssignment8.ts")]
#[case("FunctionDeclaration3.ts")]
#[case("FunctionDeclaration4.ts")]
#[case("FunctionDeclaration6.ts")]
#[case("FunctionDeclaration7.ts")]
#[case("InterfaceDeclaration8.ts")]
#[case("MemberAccessorDeclaration15.ts")]
#[case("ParameterList13.ts")]
#[case("ParameterList4.ts")]
#[case("ParameterList5.ts")]
#[case("ParameterList6.ts")]
#[case("ParameterList7.ts")]
#[case("ParameterList8.ts")]
#[case("SystemModuleForStatementNoInitializer.ts")]
#[case("abstractClassInLocalScope.ts")]
#[case("abstractClassInLocalScopeIsAbstract.ts")]
#[case("abstractIdentifierNameStrict.ts")]
#[case("abstractInterfaceIdentifierName.ts")]
#[case("abstractPropertyBasics.ts")]
#[case("abstractPropertyInConstructor.ts")]
#[case("abstractPropertyNegative.ts")]
#[case("acceptableAlias1.ts")]
#[case("accessInstanceMemberFromStaticMethod01.ts")]
#[case("accessOverriddenBaseClassMember1.ts")]
#[case("accessStaticMemberFromInstanceMethod01.ts")]
#[case("accessorAccidentalCallDiagnostic.ts")]
#[case("accessorBodyInTypeContext.ts")]
#[case("accessorDeclarationEmitVisibilityErrors.ts")] // OUT OF SCOPE transformer error
#[case("accessorParameterAccessibilityModifier.ts")]
#[case("accessorWithInitializer.ts")]
#[case("accessorWithLineTerminator.ts")]
#[case("accessorWithRestParam.ts")]
#[case("accessorWithoutBody1.ts")]
#[case("accessorWithoutBody2.ts")]
#[case("accessorsEmit.ts")]
#[case("accessorsInAmbientContext.ts")]
#[case("accessorsNotAllowedInES3.ts")]
#[case("accessors_spec_section-4.5_error-cases.ts")]
#[case("accessors_spec_section-4.5_inference.ts")]
#[case("addMoreCallSignaturesToBaseSignature.ts")]
#[case("addMoreCallSignaturesToBaseSignature2.ts")]
#[case("addMoreOverloadsToBaseSignature.ts")]
#[case("aliasAssignments.ts")] // NOT RUNNABLE
#[case("aliasBug.ts")]
#[case("aliasDoesNotDuplicateSignatures.ts")] // NOT RUNNABLE
#[case("aliasErrors.ts")]
#[case("aliasInaccessibleModule.ts")]
#[case("aliasInaccessibleModule2.ts")]
#[case("aliasOfGenericFunctionWithRestBehavedSameAsUnaliased.ts")]
#[case("aliasOnMergedModuleInterface.ts")] // NOT RUNNABLE
#[case("aliasUsageInAccessorsOfClass.ts")] // NOT RUNNABLE
#[case("aliasUsageInArray.ts")] // NOT RUNNABLE
#[case("aliasUsageInFunctionExpression.ts")] // NOT RUNNABLE
#[case("aliasUsageInGenericFunction.ts")] // NOT RUNNABLE
#[case("aliasUsageInIndexerOfClass.ts")] // NOT RUNNABLE
#[case("aliasUsageInObjectLiteral.ts")] // NOT RUNNABLE
#[case("aliasUsageInOrExpression.ts")] // NOT RUNNABLE
#[case("aliasUsageInTypeArgumentOfExtendsClause.ts")] // NOT RUNNABLE
#[case("aliasUsageInVarAssignment.ts")] // NOT RUNNABLE
#[case("aliasUsedAsNameValue.ts")] // NOT RUNNABLE
#[case("aliasWithInterfaceExportAssignmentUsedInVarInitializer.ts")] // NOT RUNNABLE
#[case("aliasesInSystemModule1.ts")]
#[case("aliasesInSystemModule2.ts")]
#[case("allowImportClausesToMergeWithTypes.ts")] // NOT RUNNABLE
#[case("allowJsClassThisTypeCrash.ts")] // NOT RUNNABLE
#[case("allowJscheckJsTypeParameterNoCrash.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports1.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports10.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports2.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports3.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports4.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports5.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports6.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports7.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports8.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImports9.ts")] // NOT RUNNABLE
#[case("allowSyntheticDefaultImportsCanPaintCrossModuleDeclaration.ts")] // NOT RUNNABLE
#[case("alwaysStrict.ts")]
#[case("alwaysStrictAlreadyUseStrict.ts")]
#[case("alwaysStrictES6.ts")]
#[case("alwaysStrictModule.ts")]
#[case("alwaysStrictModule2.ts")] // NOT RUNNABLE
#[case("alwaysStrictModule3.ts")]
#[case("alwaysStrictModule4.ts")]
#[case("alwaysStrictModule5.ts")]
#[case("alwaysStrictModule6.ts")]
#[case("alwaysStrictNoImplicitUseStrict.ts")]
#[case("ambientClassDeclarationWithExtends.ts")] // NOT RUNNABLE
#[case("ambientClassDeclaredBeforeBase.ts")] // NOT RUNNABLE
#[case("ambientClassMergesOverloadsWithInterface.ts")]
#[case("ambientClassOverloadForFunction.ts")]
#[case("ambientConstLiterals.ts")]
#[case("ambientEnum1.ts")]
#[case("ambientEnumElementInitializer1.ts")]
#[case("ambientEnumElementInitializer2.ts")]
#[case("ambientEnumElementInitializer3.ts")]
#[case("ambientEnumElementInitializer4.ts")]
#[case("ambientEnumElementInitializer5.ts")]
#[case("ambientEnumElementInitializer6.ts")]
#[case("ambientErrors1.ts")]
#[case("ambientExportDefaultErrors.ts")] // NOT RUNNABLE
#[case("ambientExternalModuleInAnotherExternalModule.ts")]
#[case("ambientExternalModuleReopen.ts")]
#[case("ambientExternalModuleWithInternalImportDeclaration.ts")] // NOT RUNNABLE
#[case("ambientExternalModuleWithRelativeExternalImportDeclaration.ts")]
#[case("ambientExternalModuleWithRelativeModuleName.ts")]
#[case("ambientExternalModuleWithoutInternalImportDeclaration.ts")] // NOT RUNNABLE
#[case("ambientFundule.ts")]
#[case("ambientGetters.ts")]
#[case("ambientModuleExports.ts")]
#[case("ambientModuleWithClassDeclarationWithExtends.ts")]
#[case("ambientModuleWithTemplateLiterals.ts")]
#[case("ambientModules.ts")]
#[case("ambientNameRestrictions.ts")]
#[case("ambientRequireFunction.ts")] // NOT RUNNABLE
#[case("ambientStatement1.ts")]
#[case("ambientWithStatements.ts")]
#[case("ambiguousCallsWhereReturnTypesAgree.ts")]
#[case("ambiguousGenericAssertion1.ts")]
#[case("ambiguousOverload.ts")]
#[case("ambiguousOverloadResolution.ts")]
#[case("amdDeclarationEmitNoExtraDeclare.ts")] // NOT RUNNABLE
#[case("amdDependencyComment1.ts")]
#[case("amdDependencyComment2.ts")]
#[case("amdDependencyCommentName1.ts")]
#[case("amdDependencyCommentName2.ts")]
#[case("amdDependencyCommentName3.ts")]
#[case("amdDependencyCommentName4.ts")]
#[case("amdModuleBundleNoDuplicateDeclarationEmitComments.ts")] // NOT RUNNABLE
#[case("amdModuleConstEnumUsage.ts")] // NOT RUNNABLE
#[case("amdModuleName1.ts")]
#[case("amdModuleName2.ts")]
#[case("anonClassDeclarationEmitIsAnon.ts")] // NOT RUNNABLE
#[case("anonterface.ts")]
#[case("anonymousClassDeclarationDoesntPrintWithReadonly.ts")]
#[case("anonymousClassExpression1.ts")]
#[case("anonymousClassExpression2.ts")]
#[case("anonymousModules.ts")]
#[case("anyAndUnknownHaveFalsyComponents.ts")]
#[case("anyAsReturnTypeForNewOnCall.ts")]
#[case("anyDeclare.ts")]
#[case("anyIdenticalToItself.ts")]
#[case("anyIndexedAccessArrayNoException.ts")]
#[case("anyInferenceAnonymousFunctions.ts")]
#[case("anyIsAssignableToObject.ts")]
#[case("anyIsAssignableToVoid.ts")]
#[case("anyMappedTypesError.ts")]
#[case("anyPlusAny1.ts")]
#[case("argsInScope.ts")]
#[case("arguments.ts")]
#[case("argumentsAsPropertyName.ts")]
#[case("argumentsAsPropertyName2.ts")]
#[case("argumentsBindsToFunctionScopeArgumentList.ts")]
#[case("argumentsObjectCreatesRestForJs.ts")] // NOT RUNNABLE
#[case("argumentsObjectIterator01_ES5.ts")]
#[case("argumentsObjectIterator01_ES6.ts")]
#[case("argumentsObjectIterator02_ES5.ts")]
#[case("argumentsObjectIterator02_ES6.ts")]
#[case("argumentsObjectIterator03_ES5.ts")]
#[case("argumentsObjectIterator03_ES6.ts")]
#[case("argumentsReferenceInConstructor1_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInConstructor2_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInConstructor3_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInConstructor4_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInConstructor5_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInConstructor6_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInConstructor7_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod1_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod2_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod3_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod4_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod5_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod6_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInMethod7_Js.ts")] // NOT RUNNABLE
#[case("argumentsReferenceInObjectLiteral_Js.ts")] // NOT RUNNABLE
#[case("argumentsUsedInClassFieldInitializerOrStaticInitializationBlock.ts")]
#[case("argumentsUsedInObjectLiteralProperty.ts")]
#[case("arithAssignTyping.ts")]
#[case("arithmeticOnInvalidTypes.ts")]
#[case("arithmeticOnInvalidTypes2.ts")]
#[case("arityErrorRelatedSpanBindingPattern.ts")]
#[case("arrayAssignmentTest1.ts")]
#[case("arrayAssignmentTest2.ts")]
#[case("arrayAssignmentTest3.ts")]
#[case("arrayAssignmentTest4.ts")]
#[case("arrayAssignmentTest5.ts")]
#[case("arrayAssignmentTest6.ts")]
#[case("arrayAugment.ts")]
#[case("arrayBestCommonTypes.ts")]
#[case("arrayBindingPatternOmittedExpressions.ts")]
#[case("arrayBufferIsViewNarrowsType.ts")]
#[case("arrayCast.ts")]
#[case("arrayConcat2.ts")]
#[case("arrayConcat3.ts")]
#[case("arrayConcatMap.ts")]
#[case("arrayConstructors1.ts")]
#[case("arrayEvery.ts")]
#[case("arrayFakeFlatNoCrashInferenceDeclarations.ts")] // OUT OF SCOPE transformer error
#[case("arrayFilter.ts")]
#[case("arrayFind.ts")]
#[case("arrayFlatMap.ts")]
#[case("arrayFlatNoCrashInference.ts")]
#[case("arrayFlatNoCrashInferenceDeclarations.ts")]
#[case("arrayFrom.ts")]
#[case("arrayIndexWithArrayFails.ts")]
#[case("arrayLiteral1.ts")]
#[case("arrayLiteral2.ts")]
#[case("arrayLiteralAndArrayConstructorEquivalence1.ts")]
#[case("arrayLiteralComments.ts")]
#[case("arrayLiteralContextualType.ts")]
#[case("arrayLiteralInNonVarArgParameter.ts")]
#[case("arrayLiteralTypeInference.ts")]
#[case("arrayOfExportedClass.ts")] // NOT RUNNABLE
#[case("arrayOfSubtypeIsAssignableToReadonlyArray.ts")]
#[case("arrayReferenceWithoutTypeArgs.ts")]
#[case("arraySigChecking.ts")]
#[case("arraySlice.ts")]
#[case("arrayTypeInSignatureOfInterfaceAndClass.ts")]
#[case("arrayconcat.ts")]
#[case("arrowFunctionErrorSpan.ts")]
#[case("arrowFunctionInConstructorArgument1.ts")]
#[case("arrowFunctionInExpressionStatement1.ts")]
#[case("arrowFunctionInExpressionStatement2.ts")]
#[case("arrowFunctionMissingCurlyWithSemicolon.ts")]
#[case("arrowFunctionWithObjectLiteralBody1.ts")]
#[case("arrowFunctionWithObjectLiteralBody2.ts")]
#[case("arrowFunctionWithObjectLiteralBody3.ts")]
#[case("arrowFunctionWithObjectLiteralBody4.ts")]
#[case("arrowFunctionWithObjectLiteralBody5.ts")]
#[case("arrowFunctionWithObjectLiteralBody6.ts")]
#[case("arrowFunctionsMissingTokens.ts")]
#[case("asiAbstract.ts")]
#[case("asiAmbientFunctionDeclaration.ts")]
#[case("asiArith.ts")]
#[case("asiBreak.ts")]
#[case("asiContinue.ts")]
#[case("asiInES6Classes.ts")]
#[case("asiPublicPrivateProtected.ts")]
#[case("asiReturn.ts")]
#[case("assertInWrapSomeTypeParameter.ts")]
#[case("assertionFunctionsCanNarrowByDiscriminant.ts")]
#[case("assign1.ts")]
#[case("assignLambdaToNominalSubtypeOfFunction.ts")]
#[case("assignToEnum.ts")]
#[case("assignToExistingClass.ts")]
#[case("assignToFn.ts")]
#[case("assignToInvalidLHS.ts")]
#[case("assignToModule.ts")]
#[case("assignToObjectTypeWithPrototypeProperty.ts")]
#[case("assignToPrototype1.ts")]
#[case("assigningFromObjectToAnythingElse.ts")]
#[case("assigningFunctionToTupleIssuesError.ts")]
#[case("assignmentCompat1.ts")]
#[case("assignmentCompatBug2.ts")]
#[case("assignmentCompatBug3.ts")]
#[case("assignmentCompatBug5.ts")]
#[case("assignmentCompatForEnums.ts")]
#[case("assignmentCompatFunctionsWithOptionalArgs.ts")]
#[case("assignmentCompatInterfaceWithStringIndexSignature.ts")]
#[case("assignmentCompatOnNew.ts")]
#[case("assignmentCompatWithOverloads.ts")]
#[case("assignmentCompatability1.ts")]
#[case("assignmentCompatability10.ts")]
#[case("assignmentCompatability11.ts")]
#[case("assignmentCompatability12.ts")]
#[case("assignmentCompatability13.ts")]
#[case("assignmentCompatability14.ts")]
#[case("assignmentCompatability15.ts")]
#[case("assignmentCompatability16.ts")]
#[case("assignmentCompatability17.ts")]
#[case("assignmentCompatability18.ts")]
#[case("assignmentCompatability19.ts")]
#[case("assignmentCompatability2.ts")]
#[case("assignmentCompatability20.ts")]
#[case("assignmentCompatability21.ts")]
#[case("assignmentCompatability22.ts")]
#[case("assignmentCompatability23.ts")]
#[case("assignmentCompatability24.ts")]
#[case("assignmentCompatability25.ts")]
#[case("assignmentCompatability26.ts")]
#[case("assignmentCompatability27.ts")]
#[case("assignmentCompatability28.ts")]
#[case("assignmentCompatability29.ts")]
#[case("assignmentCompatability3.ts")]
#[case("assignmentCompatability30.ts")]
#[case("assignmentCompatability31.ts")]
#[case("assignmentCompatability32.ts")]
#[case("assignmentCompatability33.ts")]
#[case("assignmentCompatability34.ts")]
#[case("assignmentCompatability35.ts")]
#[case("assignmentCompatability36.ts")]
#[case("assignmentCompatability37.ts")]
#[case("assignmentCompatability38.ts")]
#[case("assignmentCompatability39.ts")]
#[case("assignmentCompatability4.ts")]
#[case("assignmentCompatability40.ts")]
#[case("assignmentCompatability41.ts")]
#[case("assignmentCompatability42.ts")]
#[case("assignmentCompatability43.ts")]
#[case("assignmentCompatability44.ts")]
#[case("assignmentCompatability45.ts")]
#[case("assignmentCompatability5.ts")]
#[case("assignmentCompatability6.ts")]
#[case("assignmentCompatability7.ts")]
#[case("assignmentCompatability8.ts")]
#[case("assignmentCompatability9.ts")]
#[case("assignmentCompatability_checking-apply-member-off-of-function-interface.ts")]
#[case("assignmentCompatability_checking-call-member-off-of-function-interface.ts")]
#[case("assignmentCompatibilityForConstrainedTypeParameters.ts")]
#[case("assignmentIndexedToPrimitives.ts")]
#[case("assignmentNestedInLiterals.ts")]
#[case("assignmentNonObjectTypeConstraints.ts")]
#[case("assignmentRestElementWithErrorSourceType.ts")]
#[case("assignmentStricterConstraints.ts")]
#[case("assignmentToExpandingArrayType.ts")]
#[case("assignmentToFunction.ts")]
#[case("assignmentToObject.ts")]
#[case("assignmentToObjectAndFunction.ts")]
#[case("assignmentToParenthesizedExpression1.ts")]
#[case("assignmentToReferenceTypes.ts")]
#[case("asyncArrowInClassES5.ts")]
#[case("asyncAwaitWithCapturedBlockScopeVar.ts")]
#[case("asyncFunctionContextuallyTypedReturns.ts")]
#[case("asyncFunctionNoReturnType.ts")]
#[case("asyncFunctionReturnExpressionErrorSpans.ts")]
#[case("asyncFunctionReturnType.ts")]
#[case("asyncFunctionTempVariableScoping.ts")]
#[case("asyncFunctionWithForStatementNoInitializer.ts")]
#[case("asyncFunctionsAcrossFiles.ts")] // NOT RUNNABLE
#[case("asyncFunctionsAndStrictNullChecks.ts")]
#[case("asyncIIFE.ts")]
#[case("asyncImportNestedYield.ts")]
#[case("augmentArray.ts")]
#[case("augmentExportEquals1.ts")] // NOT RUNNABLE
#[case("augmentExportEquals1_1.ts")] // NOT RUNNABLE
#[case("augmentExportEquals2.ts")] // NOT RUNNABLE
#[case("augmentExportEquals2_1.ts")] // NOT RUNNABLE
#[case("augmentExportEquals3.ts")] // NOT RUNNABLE
#[case("augmentExportEquals3_1.ts")] // NOT RUNNABLE
#[case("augmentExportEquals4.ts")] // NOT RUNNABLE
#[case("augmentExportEquals4_1.ts")] // NOT RUNNABLE
#[case("augmentExportEquals5.ts")] // NOT RUNNABLE
#[case("augmentExportEquals6.ts")] // NOT RUNNABLE
#[case("augmentExportEquals6_1.ts")] // NOT RUNNABLE
#[case("augmentExportEquals7.ts")] // NOT RUNNABLE
#[case("augmentedClassWithPrototypePropertyOnModule.ts")]
#[case("augmentedTypeBracketNamedPropertyAccess.ts")]
#[case("augmentedTypesClass.ts")]
#[case("augmentedTypesClass2.ts")]
#[case("augmentedTypesClass2a.ts")]
#[case("augmentedTypesClass3.ts")]
#[case("augmentedTypesClass4.ts")]
#[case("augmentedTypesEnum.ts")]
#[case("augmentedTypesEnum2.ts")]
#[case("augmentedTypesEnum3.ts")]
#[case("augmentedTypesExternalModule1.ts")]
#[case("augmentedTypesFunction.ts")]
#[case("augmentedTypesInterface.ts")]
#[case("augmentedTypesModules.ts")]
#[case("augmentedTypesModules2.ts")]
#[case("augmentedTypesModules3.ts")]
#[case("augmentedTypesModules3b.ts")]
#[case("augmentedTypesModules4.ts")]
#[case("augmentedTypesVar.ts")]
#[case("autoAsiForStaticsInClassDeclaration.ts")]
#[case("autoLift2.ts")]
#[case("autolift3.ts")]
#[case("autolift4.ts")]
#[case("autonumberingInEnums.ts")]
#[case("avoid.ts")]
#[case("awaitExpressionInnerCommentEmit.ts")]
#[case("awaitInClassInAsyncFunction.ts")]
#[case("awaitInNonAsyncFunction.ts")]
#[case("awaitLiteralValues.ts")]
#[case("awaitUnionPromise.ts")]
#[case("awaitedType.ts")]
#[case("awaitedTypeStrictNull.ts")]
#[case("badArrayIndex.ts")]
#[case("badArraySyntax.ts")]
#[case("badExternalModuleReference.ts")]
#[case("badInferenceLowerPriorityThanGoodInference.ts")] // FAILING 408
#[case("badOverloadError.ts")]
#[case("badThisBinding.ts")]
#[case("bangInModuleName.ts")] // NOT RUNNABLE
#[case("baseCheck.ts")]
#[case("baseClassImprovedMismatchErrors.ts")]
#[case("baseConstraintOfDecorator.ts")]
#[case("baseExpressionTypeParameters.ts")]
#[case("baseIndexSignatureResolution.ts")]
#[case("baseTypeAfterDerivedType.ts")]
#[case("baseTypeOrderChecking.ts")]
#[case("baseTypePrivateMemberClash.ts")]
#[case("baseTypeWrappingInstantiationChain.ts")]
#[case("bases.ts")]
#[case("bestChoiceType.ts")]
#[case("bestCommonTypeReturnStatement.ts")]
#[case("bestCommonTypeWithContextualTyping.ts")]
#[case("bestCommonTypeWithOptionalProperties.ts")]
#[case("betterErrorForAccidentalCall.ts")]
#[case("betterErrorForUnionCall.ts")]
#[case("bigIntWithTargetES2016.ts")]
#[case("bigIntWithTargetES3.ts")]
#[case("bigIntWithTargetLessThanES2016.ts")]
#[case("bigint64ArraySubarray.ts")]
#[case("bigintIndex.ts")] // NOT RUNNABLE
#[case("bigintWithLib.ts")]
#[case("bigintWithoutLib.ts")]
#[case("binaryArithmatic1.ts")]
#[case("binaryArithmatic2.ts")]
#[case("binaryArithmatic3.ts")]
#[case("binaryArithmatic4.ts")]
#[case("binaryArithmeticControlFlowGraphNotTooLarge.ts")]
#[case("bind1.ts")]
#[case("bind2.ts")]
#[case("binderBinaryExpressionStress.ts")] // NOT RUNNABLE
#[case("binderBinaryExpressionStressJs.ts")] // NOT RUNNABLE
#[case("bindingPatternInParameter01.ts")]
#[case("bindingPatternOmittedExpressionNesting.ts")]
#[case("binopAssignmentShouldHaveType.ts")]
#[case("bitwiseCompoundAssignmentOperators.ts")]
#[case("blockScopedBindingCaptureThisInFunction.ts")]
#[case("blockScopedBindingUsedBeforeDef.ts")]
#[case("blockScopedBindingsInDownlevelGenerator.ts")]
#[case("blockScopedBindingsReassignedInLoop1.ts")]
#[case("blockScopedBindingsReassignedInLoop2.ts")]
#[case("blockScopedBindingsReassignedInLoop3.ts")]
#[case("blockScopedBindingsReassignedInLoop4.ts")]
#[case("blockScopedBindingsReassignedInLoop5.ts")]
#[case("blockScopedBindingsReassignedInLoop6.ts")]
#[case("blockScopedClassDeclarationAcrossFiles.ts")] // NOT RUNNABLE
#[case("blockScopedEnumVariablesUseBeforeDef.ts")]
#[case("blockScopedEnumVariablesUseBeforeDef_preserve.ts")]
#[case("blockScopedFunctionDeclarationES5.ts")]
#[case("blockScopedFunctionDeclarationES6.ts")]
#[case("blockScopedFunctionDeclarationInStrictClass.ts")]
#[case("blockScopedFunctionDeclarationInStrictModule.ts")]
#[case("blockScopedFunctionDeclarationStrictES5.ts")]
#[case("blockScopedFunctionDeclarationStrictES6.ts")]
#[case("blockScopedNamespaceDifferentFile.ts")] // NOT RUNNABLE
#[case("blockScopedSameNameFunctionDeclarationES5.ts")]
#[case("blockScopedSameNameFunctionDeclarationES6.ts")]
#[case("blockScopedSameNameFunctionDeclarationStrictES5.ts")]
#[case("blockScopedSameNameFunctionDeclarationStrictES6.ts")]
#[case("blockScopedVariablesUseBeforeDef.ts")]
#[case("bluebirdStaticThis.ts")]
#[case("bom-utf16be.ts")]
#[case("bom-utf16le.ts")]
#[case("bom-utf8.ts")]
#[case("booleanAssignment.ts")]
#[case("booleanFilterAnyArray.ts")]
#[case("booleanLiteralsContextuallyTypedFromUnion.tsx")] // NOT RUNNABLE
#[case("breakInIterationOrSwitchStatement1.ts")]
#[case("breakInIterationOrSwitchStatement2.ts")]
#[case("breakInIterationOrSwitchStatement3.ts")]
#[case("breakInIterationOrSwitchStatement4.ts")]
#[case("breakNotInIterationOrSwitchStatement1.ts")]
#[case("breakNotInIterationOrSwitchStatement2.ts")]
#[case("breakTarget1.ts")]
#[case("breakTarget2.ts")]
#[case("breakTarget3.ts")]
#[case("breakTarget4.ts")]
#[case("breakTarget5.ts")]
#[case("breakTarget6.ts")]
#[case("bundledDtsLateExportRenaming.ts")] // NOT RUNNABLE
#[case("cacheResolutions.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution1.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution2.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution3.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution4.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution5.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution6.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution7.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution8.ts")] // NOT RUNNABLE
#[case("cachedModuleResolution9.ts")] // NOT RUNNABLE
#[case("callConstructAssignment.ts")]
#[case("callExpressionWithMissingTypeArgument1.ts")]
#[case("callExpressionWithTypeParameterConstrainedToOuterTypeParameter.ts")]
#[case("callOfConditionalTypeWithConcreteBranches.ts")]
#[case("callOnClass.ts")]
#[case("callOnInstance.ts")]
#[case("callOverloadViaElementAccessExpression.ts")]
#[case("callOverloads1.ts")]
#[case("callOverloads2.ts")]
#[case("callOverloads3.ts")]
#[case("callOverloads4.ts")]
#[case("callOverloads5.ts")]
#[case("callSignatureFunctionOverload.ts")]
#[case("callSignaturesShouldBeResolvedBeforeSpecialization.ts")]
#[case("callWithWrongNumberOfTypeArguments.ts")]
#[case("callbackArgsDifferByOptionality.ts")]
#[case("callbacksDontShareTypes.ts")]
#[case("callsOnComplexSignatures.tsx")] // NOT RUNNABLE on .lib directive
#[case("cannotInvokeNewOnErrorExpression.ts")]
#[case("cannotInvokeNewOnIndexExpression.ts")]
#[case("captureSuperPropertyAccessInSuperCall01.ts")]
#[case("captureThisInSuperCall.ts")]
#[case("capturedLetConstInLoop1.ts")]
#[case("capturedLetConstInLoop10.ts")]
#[case("capturedLetConstInLoop10_ES6.ts")]
#[case("capturedLetConstInLoop11.ts")]
#[case("capturedLetConstInLoop11_ES6.ts")]
#[case("capturedLetConstInLoop12.ts")]
#[case("capturedLetConstInLoop13.ts")]
#[case("capturedLetConstInLoop1_ES6.ts")]
#[case("capturedLetConstInLoop2.ts")]
#[case("capturedLetConstInLoop2_ES6.ts")]
#[case("capturedLetConstInLoop3.ts")]
#[case("capturedLetConstInLoop3_ES6.ts")]
#[case("capturedLetConstInLoop4.ts")]
#[case("capturedLetConstInLoop4_ES6.ts")]
#[case("capturedLetConstInLoop5.ts")]
#[case("capturedLetConstInLoop5_ES6.ts")]
#[case("capturedLetConstInLoop6.ts")]
#[case("capturedLetConstInLoop6_ES6.ts")]
#[case("capturedLetConstInLoop7.ts")]
#[case("capturedLetConstInLoop7_ES6.ts")]
#[case("capturedLetConstInLoop8.ts")]
#[case("capturedLetConstInLoop8_ES6.ts")]
#[case("capturedLetConstInLoop9.ts")]
#[case("capturedLetConstInLoop9_ES6.ts")]
#[case("capturedParametersInInitializers1.ts")]
#[case("capturedParametersInInitializers2.ts")]
#[case("capturedVarInLoop.ts")]
#[case("caseInsensitiveFileSystemWithCapsImportTypeDeclarations.ts")] // NOT RUNNABLE
#[case("castExpressionParentheses.ts")]
#[case("castFunctionExpressionShouldBeParenthesized.ts")]
#[case("castNewObjectBug.ts")]
#[case("castOfAwait.ts")]
#[case("castOfYield.ts")]
#[case("castParentheses.ts")]
#[case("castTest.ts")]
#[case("catch.ts")]
#[case("catchClauseWithInitializer1.ts")]
#[case("cf.ts")]
#[case("chainedAssignment1.ts")]
#[case("chainedAssignment2.ts")]
#[case("chainedAssignment3.ts")]
#[case("chainedAssignmentChecking.ts")]
#[case("chainedCallsWithTypeParameterConstrainedToOtherTypeParameter.ts")]
#[case("chainedCallsWithTypeParameterConstrainedToOtherTypeParameter2.ts")]
#[case("chainedImportAlias.ts")] // NOT RUNNABLE
#[case("chainedSpecializationToObjectTypeLiteral.ts")]
#[case("checkDestructuringShorthandAssigment.ts")] // NOT RUNNABLE
#[case("checkDestructuringShorthandAssigment2.ts")]
#[case("checkForObjectTooStrict.ts")]
#[case("checkIndexConstraintOfJavascriptClassExpression.ts")] // NOT RUNNABLE
#[case("checkInfiniteExpansionTermination.ts")]
#[case("checkInfiniteExpansionTermination2.ts")]
#[case("checkInterfaceBases.ts")] // NOT RUNNABLE
#[case("checkJsFiles.ts")] // NOT RUNNABLE
#[case("checkJsFiles2.ts")] // NOT RUNNABLE
#[case("checkJsFiles3.ts")] // NOT RUNNABLE
#[case("checkJsFiles4.ts")] // NOT RUNNABLE
#[case("checkJsFiles5.ts")] // NOT RUNNABLE
#[case("checkJsFiles6.ts")] // NOT RUNNABLE
#[case("checkJsFiles7.ts")] // NOT RUNNABLE
#[case("checkJsFiles_noErrorLocation.ts")] // NOT RUNNABLE
#[case("checkJsFiles_skipDiagnostics.ts")] // NOT RUNNABLE
#[case("checkJsObjectLiteralHasCheckedKeyof.ts")] // NOT RUNNABLE
#[case("checkJsObjectLiteralIndexSignatures.ts")] // NOT RUNNABLE
#[case("checkJsTypeDefNoUnusedLocalMarked.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment1.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment2.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment3.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment4.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment5.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment6.ts")] // NOT RUNNABLE
#[case("checkJsdocTypeTagOnExportAssignment7.ts")] // NOT RUNNABLE
#[case("checkMergedGlobalUMDSymbol.ts")] // NOT RUNNABLE
#[case("checkSuperCallBeforeThisAccess.ts")]
#[case("checkSuperCallBeforeThisAccessing1.ts")]
#[case("checkSuperCallBeforeThisAccessing2.ts")]
#[case("checkSuperCallBeforeThisAccessing3.ts")]
#[case("checkSuperCallBeforeThisAccessing4.ts")]
#[case("checkSuperCallBeforeThisAccessing5.ts")]
#[case("checkSuperCallBeforeThisAccessing6.ts")]
#[case("checkSuperCallBeforeThisAccessing7.ts")]
#[case("checkSuperCallBeforeThisAccessing8.ts")]
#[case("checkSuperCallBeforeThisAccessing9.ts")] // NOT RUNNABLE
#[case("checkSwitchStatementIfCaseTypeIsString.ts")]
#[case("checkTypePredicateForRedundantProperties.ts")]
#[case("circularBaseTypes.ts")]
#[case("circularConstrainedMappedTypeNoCrash.ts")]
#[case("circularConstraintYieldsAppropriateError.ts")]
#[case("circularContextualMappedType.ts")]
#[case("circularContextualReturnType.ts")]
#[case("circularInferredTypeOfVariable.ts")]
#[case("circularModuleImports.ts")]
#[case("circularObjectLiteralAccessors.ts")]
#[case("circularOptionalityRemoval.ts")]
#[case("circularReferenceInImport.ts")] // NOT RUNNABLE
#[case("circularTypeofWithFunctionModule.ts")]
#[case("circularlyConstrainedMappedTypeContainingConditionalNoInfiniteInstantiationDepth.ts")] // FAILING 620
#[case("circularlyReferentialInterfaceAccessNoCrash.ts")]
#[case("circularlySimplifyingConditionalTypesNoCrash.ts")]
#[case("class2.ts")]
#[case("classAttributeInferenceTemplate.ts")]
#[case("classAttributeInferenceTemplateJS.ts")] // NOT RUNNABLE
#[case("classBlockScoping.ts")]
#[case("classCannotExtendVar.ts")]
#[case("classDeclarationBlockScoping1.ts")]
#[case("classDeclarationBlockScoping2.ts")]
#[case("classDeclarationCheckUsedBeforeDefinitionInFunctionDeclaration.ts")]
#[case("classDeclarationCheckUsedBeforeDefinitionInItself.ts")]
#[case("classDeclarationMergedInModuleWithContinuation.ts")]
#[case("classDeclarationShouldBeOutOfScopeInComputedNames.ts")]
#[case("classDeclaredBeforeClassFactory.ts")]
#[case("classExpressionAssignment.ts")]
#[case("classExpressionExtendingAbstractClass.ts")]
#[case("classExpressionInClassStaticDeclarations.ts")]
#[case("classExpressionNames.ts")] // NOT RUNNABLE
#[case("classExpressionPropertyModifiers.ts")] // NOT RUNNABLE
#[case("classExpressionTest1.ts")]
#[case("classExpressionTest2.ts")]
#[case("classExpressionWithDecorator1.ts")]
#[case("classExpressionWithResolutionOfNamespaceOfSameName01.ts")]
#[case("classExpressionWithStaticProperties1.ts")]
#[case("classExpressionWithStaticProperties2.ts")]
#[case("classExpressionWithStaticProperties3.ts")]
#[case("classExpressionWithStaticPropertiesES61.ts")]
#[case("classExpressionWithStaticPropertiesES62.ts")]
#[case("classExpressionWithStaticPropertiesES63.ts")]
#[case("classExpressionWithStaticPropertiesES64.ts")]
#[case("classExpressions.ts")]
#[case("classExtendingAny.ts")] // NOT RUNNABLE
#[case("classExtendingQualifiedName.ts")]
#[case("classExtendingQualifiedName2.ts")]
#[case("classExtendsAcrossFiles.ts")] // NOT RUNNABLE
#[case("classExtendsClauseClassMergedWithModuleNotReferingConstructor.ts")]
#[case("classExtendsClauseClassNotReferringConstructor.ts")]
#[case("classExtendsInterface.ts")]
#[case("classExtendsInterfaceInExpression.ts")]
#[case("classExtendsInterfaceInModule.ts")]
#[case("classExtendsInterfaceThatExtendsClassWithPrivates1.ts")]
#[case("classExtendsInterface_not.ts")]
#[case("classExtendsMultipleBaseClasses.ts")]
#[case("classExtendsNull.ts")]
#[case("classExtensionNameOutput.ts")]
#[case("classFunctionMerging.ts")]
#[case("classHeritageWithTrailingSeparator.ts")]
#[case("classImplementingInterfaceIndexer.ts")]
#[case("classImplementsClass1.ts")]
#[case("classImplementsClass2.ts")]
#[case("classImplementsClass3.ts")]
#[case("classImplementsClass4.ts")]
#[case("classImplementsClass5.ts")]
#[case("classImplementsClass6.ts")]
#[case("classImplementsClass7.ts")]
#[case("classImplementsImportedInterface.ts")]
#[case("classInConvertedLoopES5.ts")]
#[case("classIndexer.ts")]
#[case("classIndexer2.ts")]
#[case("classIndexer3.ts")]
#[case("classIndexer4.ts")]
#[case("classIndexer5.ts")]
#[case("classInheritence.ts")]
#[case("classMemberInitializerScoping.ts")]
#[case("classMemberInitializerWithLamdaScoping.ts")]
#[case("classMemberInitializerWithLamdaScoping2.ts")] // NOT RUNNABLE
#[case("classMemberInitializerWithLamdaScoping3.ts")] // NOT RUNNABLE
#[case("classMemberInitializerWithLamdaScoping4.ts")] // NOT RUNNABLE
#[case("classMemberInitializerWithLamdaScoping5.ts")]
#[case("classMemberWithMissingIdentifier.ts")]
#[case("classMemberWithMissingIdentifier2.ts")]
#[case("classMergedWithInterfaceMultipleBasesNoError.ts")]
#[case("classMethodWithKeywordName1.ts")]
#[case("classOrder1.ts")]
#[case("classOrder2.ts")]
#[case("classOrderBug.ts")]
#[case("classOverloadForFunction.ts")]
#[case("classOverloadForFunction2.ts")]
#[case("classPropertyErrorOnNameOnly.ts")]
#[case("classSideInheritance1.ts")]
#[case("classSideInheritance2.ts")]
#[case("classSideInheritance3.ts")]
#[case("classStaticInitializersUsePropertiesBeforeDeclaration.ts")]
#[case("classStaticPropertyAccess.ts")]
#[case("classStaticPropertyTypeGuard.ts")]
#[case("classTypeParametersInStatics.ts")]
#[case("classUpdateTests.ts")]
#[case("classUsedBeforeInitializedVariables.ts")]
#[case("classWithDuplicateIdentifier.ts")]
#[case("classWithEmptyTypeParameter.ts")]
#[case("classWithMultipleBaseClasses.ts")]
#[case("classWithOverloadImplementationOfWrongName.ts")]
#[case("classWithOverloadImplementationOfWrongName2.ts")]
#[case("classdecl.ts")]
#[case("clinterfaces.ts")]
#[case("cloduleAcrossModuleDefinitions.ts")]
#[case("cloduleAndTypeParameters.ts")]
#[case("cloduleGenericOnSelfMember.ts")]
#[case("cloduleSplitAcrossFiles.ts")] // NOT RUNNABLE
#[case("cloduleStaticMembers.ts")]
#[case("cloduleTest1.ts")]
#[case("cloduleTest2.ts")]
#[case("cloduleWithDuplicateMember1.ts")]
#[case("cloduleWithDuplicateMember2.ts")]
#[case("cloduleWithPriorInstantiatedModule.ts")]
#[case("cloduleWithPriorUninstantiatedModule.ts")]
#[case("cloduleWithRecursiveReference.ts")]
#[case("clodulesDerivedClasses.ts")]
#[case("coAndContraVariantInferences.ts")]
#[case("collectionPatternNoError.ts")]
#[case("collisionArgumentsArrowFunctions.ts")]
#[case("collisionArgumentsClassConstructor.ts")]
#[case("collisionArgumentsClassMethod.ts")]
#[case("collisionArgumentsFunction.ts")]
#[case("collisionArgumentsFunctionExpressions.ts")]
#[case("collisionArgumentsInType.ts")]
#[case("collisionArgumentsInterfaceMembers.ts")]
#[case("collisionCodeGenEnumWithEnumMemberConflict.ts")]
#[case("collisionCodeGenModuleWithAccessorChildren.ts")]
#[case("collisionCodeGenModuleWithConstructorChildren.ts")]
#[case("collisionCodeGenModuleWithEnumMemberConflict.ts")]
#[case("collisionCodeGenModuleWithFunctionChildren.ts")]
#[case("collisionCodeGenModuleWithMemberClassConflict.ts")]
#[case("collisionCodeGenModuleWithMemberInterfaceConflict.ts")]
#[case("collisionCodeGenModuleWithMemberVariable.ts")]
#[case("collisionCodeGenModuleWithMethodChildren.ts")]
#[case("collisionCodeGenModuleWithModuleChildren.ts")]
#[case("collisionCodeGenModuleWithModuleReopening.ts")]
#[case("collisionCodeGenModuleWithPrivateMember.ts")]
#[case("collisionCodeGenModuleWithUnicodeNames.ts")]
#[case("collisionExportsRequireAndAlias.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndAmbientClass.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndAmbientEnum.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndAmbientFunction.ts")]
#[case("collisionExportsRequireAndAmbientFunctionInGlobalFile.ts")]
#[case("collisionExportsRequireAndAmbientModule.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndAmbientVar.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndClass.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndEnum.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndFunction.ts")]
#[case("collisionExportsRequireAndFunctionInGlobalFile.ts")]
#[case("collisionExportsRequireAndInternalModuleAlias.ts")]
#[case("collisionExportsRequireAndInternalModuleAliasInGlobalFile.ts")]
#[case("collisionExportsRequireAndModule.ts")] // NOT RUNNABLE
#[case("collisionExportsRequireAndUninstantiatedModule.ts")]
#[case("collisionExportsRequireAndVar.ts")] // NOT RUNNABLE
#[case("collisionRestParameterArrowFunctions.ts")]
#[case("collisionRestParameterClassConstructor.ts")]
#[case("collisionRestParameterClassMethod.ts")]
#[case("collisionRestParameterFunction.ts")]
#[case("collisionRestParameterFunctionExpressions.ts")]
#[case("collisionRestParameterInType.ts")]
#[case("collisionRestParameterInterfaceMembers.ts")]
#[case("collisionRestParameterUnderscoreIUsage.ts")]
#[case("collisionSuperAndLocalFunctionInAccessors.ts")]
#[case("collisionSuperAndLocalFunctionInConstructor.ts")]
#[case("collisionSuperAndLocalFunctionInMethod.ts")]
#[case("collisionSuperAndLocalFunctionInProperty.ts")]
#[case("collisionSuperAndLocalVarInAccessors.ts")]
#[case("collisionSuperAndLocalVarInConstructor.ts")]
#[case("collisionSuperAndLocalVarInMethod.ts")]
#[case("collisionSuperAndLocalVarInProperty.ts")]
#[case("collisionSuperAndNameResolution.ts")]
#[case("collisionSuperAndParameter.ts")]
#[case("collisionSuperAndParameter1.ts")]
#[case("collisionSuperAndPropertyNameAsConstuctorParameter.ts")]
#[case("collisionThisExpressionAndAliasInGlobal.ts")]
#[case("collisionThisExpressionAndAmbientClassInGlobal.ts")]
#[case("collisionThisExpressionAndAmbientVarInGlobal.ts")]
#[case("collisionThisExpressionAndClassInGlobal.ts")]
#[case("collisionThisExpressionAndEnumInGlobal.ts")]
#[case("collisionThisExpressionAndFunctionInGlobal.ts")]
#[case("collisionThisExpressionAndLocalVarInAccessors.ts")]
#[case("collisionThisExpressionAndLocalVarInConstructor.ts")]
#[case("collisionThisExpressionAndLocalVarInFunction.ts")]
#[case("collisionThisExpressionAndLocalVarInLambda.ts")]
#[case("collisionThisExpressionAndLocalVarInMethod.ts")]
#[case("collisionThisExpressionAndLocalVarInProperty.ts")]
#[case("collisionThisExpressionAndLocalVarWithSuperExperssion.ts")]
#[case("collisionThisExpressionAndModuleInGlobal.ts")]
#[case("collisionThisExpressionAndNameResolution.ts")]
#[case("collisionThisExpressionAndParameter.ts")]
#[case("collisionThisExpressionAndPropertyNameAsConstuctorParameter.ts")]
#[case("collisionThisExpressionAndVarInGlobal.ts")]
#[case("commaOperator1.ts")]
#[case("commaOperatorInConditionalExpression.ts")]
#[case("commaOperatorLeftSideUnused.ts")]
#[case("commentBeforeStaticMethod1.ts")]
#[case("commentEmitAtEndOfFile1.ts")]
#[case("commentEmitWithCommentOnLastLine.ts")]
#[case("commentInEmptyParameterList1.ts")]
#[case("commentInMethodCall.ts")]
#[case("commentInNamespaceDeclarationWithIdentifierPathName.ts")]
#[case("commentLeadingCloseBrace.ts")]
#[case("commentOnAmbientClass1.ts")] // NOT RUNNABLE
#[case("commentOnAmbientEnum.ts")] // NOT RUNNABLE
#[case("commentOnAmbientModule.ts")] // NOT RUNNABLE
#[case("commentOnAmbientVariable1.ts")]
#[case("commentOnAmbientVariable2.ts")] // NOT RUNNABLE
#[case("commentOnAmbientfunction.ts")] // NOT RUNNABLE
#[case("commentOnArrayElement1.ts")]
#[case("commentOnArrayElement10.ts")]
#[case("commentOnArrayElement11.ts")]
#[case("commentOnArrayElement12.ts")]
#[case("commentOnArrayElement13.ts")]
#[case("commentOnArrayElement14.ts")]
#[case("commentOnArrayElement15.ts")]
#[case("commentOnArrayElement16.ts")]
#[case("commentOnArrayElement2.ts")]
#[case("commentOnArrayElement3.ts")]
#[case("commentOnArrayElement4.ts")]
#[case("commentOnArrayElement5.ts")]
#[case("commentOnArrayElement6.ts")]
#[case("commentOnArrayElement7.ts")]
#[case("commentOnArrayElement8.ts")]
#[case("commentOnArrayElement9.ts")]
#[case("commentOnBinaryOperator1.ts")]
#[case("commentOnBinaryOperator2.ts")]
#[case("commentOnBlock1.ts")]
#[case("commentOnClassAccessor1.ts")]
#[case("commentOnClassAccessor2.ts")]
#[case("commentOnClassMethod1.ts")]
#[case("commentOnDecoratedClassDeclaration.ts")]
#[case("commentOnElidedModule1.ts")] // NOT RUNNABLE
#[case("commentOnExportEnumDeclaration.ts")]
#[case("commentOnExpressionStatement1.ts")]
#[case("commentOnIfStatement1.ts")]
#[case("commentOnImportStatement1.ts")]
#[case("commentOnImportStatement2.ts")]
#[case("commentOnImportStatement3.ts")]
#[case("commentOnInterface1.ts")] // NOT RUNNABLE
#[case("commentOnParameter1.ts")]
#[case("commentOnParameter2.ts")]
#[case("commentOnParameter3.ts")]
#[case("commentOnParenthesizedExpressionOpenParen1.ts")]
#[case("commentOnSignature1.ts")] // NOT RUNNABLE
#[case("commentOnSimpleArrowFunctionBody1.ts")]
#[case("commentOnStaticMember1.ts")]
#[case("commentWithUnreasonableIndentationLevel01.ts")]
#[case("commentsAfterCaseClauses1.ts")]
#[case("commentsAfterCaseClauses2.ts")]
#[case("commentsAfterCaseClauses3.ts")]
#[case("commentsAfterFunctionExpression1.ts")]
#[case("commentsAfterSpread.ts")]
#[case("commentsArgumentsOfCallExpression1.ts")]
#[case("commentsArgumentsOfCallExpression2.ts")]
#[case("commentsAtEndOfFile1.ts")]
#[case("commentsBeforeFunctionExpression1.ts")]
#[case("commentsBeforeVariableStatement1.ts")]
#[case("commentsClass.ts")]
#[case("commentsClassMembers.ts")]
#[case("commentsCommentParsing.ts")]
#[case("commentsDottedModuleName.ts")]
#[case("commentsEnums.ts")]
#[case("commentsExternalModules.ts")] // NOT RUNNABLE
#[case("commentsExternalModules2.ts")] // NOT RUNNABLE
#[case("commentsExternalModules3.ts")] // NOT RUNNABLE
#[case("commentsFormatting.ts")]
#[case("commentsFunction.ts")]
#[case("commentsInheritance.ts")]
#[case("commentsInterface.ts")]
#[case("commentsModules.ts")]
#[case("commentsMultiModuleMultiFile.ts")] // NOT RUNNABLE
#[case("commentsMultiModuleSingleFile.ts")]
#[case("commentsOnJSXExpressionsArePreserved.tsx")] // NOT RUNNABLE
#[case("commentsOnObjectLiteral1.ts")]
#[case("commentsOnObjectLiteral2.ts")]
#[case("commentsOnObjectLiteral3.ts")]
#[case("commentsOnObjectLiteral4.ts")]
#[case("commentsOnPropertyOfObjectLiteral1.ts")]
#[case("commentsOnRequireStatement.ts")] // NOT RUNNABLE
#[case("commentsOnReturnStatement1.ts")]
#[case("commentsOnStaticMembers.ts")]
#[case("commentsOverloads.ts")]
#[case("commentsPropertySignature1.ts")]
#[case("commentsTypeParameters.ts")]
#[case("commentsVarDecl.ts")]
#[case("commentsVariableStatement1.ts")]
#[case("commentsdoNotEmitComments.ts")]
#[case("commentsemitComments.ts")]
#[case("commonJsExportTypeDeclarationError.ts")] // NOT RUNNABLE
#[case("commonJsImportClassExpression.ts")] // NOT RUNNABLE
#[case("commonJsIsolatedModules.ts")] // NOT RUNNABLE
#[case("commonJsUnusedLocals.ts")] // NOT RUNNABLE
#[case("commonMissingSemicolons.ts")] // NOT RUNNABLE
#[case("commonSourceDir1.ts")] // NOT RUNNABLE
#[case("commonSourceDir2.ts")] // NOT RUNNABLE
#[case("commonSourceDir3.ts")] // NOT RUNNABLE
#[case("commonSourceDir4.ts")] // NOT RUNNABLE
#[case("commonSourceDir5.ts")] // NOT RUNNABLE
#[case("commonSourceDir6.ts")] // NOT RUNNABLE
#[case("commonSourceDirectory.ts")] // NOT RUNNABLE
#[case("commonSourceDirectory_dts.ts")] // NOT RUNNABLE
#[case("commonjsAccessExports.ts")] // NOT RUNNABLE
#[case("commonjsSafeImport.ts")] // NOT RUNNABLE
#[case("compareTypeParameterConstrainedByLiteralToLiteral.ts")]
#[case("comparisonOfPartialDeepAndIndexedAccessTerminatesWithoutError.ts")]
#[case("compilerOptionsDeclarationAndNoEmit.ts")] // NOT RUNNABLE
#[case("compilerOptionsOutAndNoEmit.ts")] // NOT RUNNABLE
#[case("compilerOptionsOutDirAndNoEmit.ts")] // NOT RUNNABLE
#[case("compilerOptionsOutFileAndNoEmit.ts")] // NOT RUNNABLE
#[case("complexClassRelationships.ts")]
#[case("complexNarrowingWithAny.ts")]
#[case("complexRecursiveCollections.ts")] // NOT RUNNABLE
#[case("complicatedGenericRecursiveBaseClassReference.ts")]
#[case("complicatedIndexedAccessKeyofReliesOnKeyofNeverUpperBound.ts")]
#[case("complicatedIndexesOfIntersectionsAreInferencable.ts")]
#[case("complicatedPrivacy.ts")]
#[case("compositeGenericFunction.ts")]
#[case("compositeWithNodeModulesSourceFile.ts")] // NOT RUNNABLE
#[case("compoundVarDecl1.ts")]
#[case("computedPropertiesInDestructuring1.ts")]
#[case("computedPropertiesInDestructuring1_ES6.ts")]
#[case("computedPropertiesInDestructuring2.ts")]
#[case("computedPropertiesInDestructuring2_ES6.ts")]
#[case("computedPropertiesTransformedInOtherwiseNonTSClasses.ts")]
#[case("computedTypesKeyofNoIndexSignatureType.ts")]
#[case("computerPropertiesInES5ShouldBeTransformed.ts")]
#[case("concatClassAndString.ts")]
#[case("concatError.ts")]
#[case("concatTuples.ts")]
#[case("conditionalAnyCheckTypePicksBothBranches.ts")]
#[case("conditionalEqualityTestingNullability.ts")]
#[case("conditionalExpression1.ts")]
// this seems to be "non-deterministic" in terms of whether the indented error text shows `number` vs `string` (that appears to be controlled by type ID ordering via getUnionType() and I saw different results when running manually with/without --skipLibCheck for example)
#[case("conditionalExpressionNewLine1.ts")]
#[case("conditionalExpressionNewLine10.ts")]
#[case("conditionalExpressionNewLine2.ts")]
#[case("conditionalExpressionNewLine3.ts")]
#[case("conditionalExpressionNewLine4.ts")]
#[case("conditionalExpressionNewLine5.ts")]
#[case("conditionalExpressionNewLine6.ts")]
#[case("conditionalExpressionNewLine7.ts")]
#[case("conditionalExpressionNewLine8.ts")]
#[case("conditionalExpressionNewLine9.ts")]
#[case("conditionalExpressions2.ts")]
#[case("conditionalTypeAssignabilityWhenDeferred.ts")]
#[case("conditionalTypeClassMembers.ts")]
#[case("conditionalTypeContextualTypeSimplificationsSuceeds.ts")]
#[case("conditionalTypeDiscriminatingLargeUnionRegularTypeFetchingSpeedReasonable.ts")]
#[case("conditionalTypeDoesntSpinForever.ts")]
#[case("conditionalTypeGenericInSignatureTypeParameterConstraint.ts")]
#[case("conditionalTypeRelaxingConstraintAssignability.ts")]
#[case("conditionalTypeSimplification.ts")]
#[case("conditionalTypeSubclassExtendsTypeParam.ts")]
#[case("conditionalTypeVarianceBigArrayConstraintsPerformance.ts")] // NOT RUNNABLE on .lib directive
#[case("conditionalTypesASI.ts")]
#[case("conditionalTypesSimplifyWhenTrivial.ts")]
#[case("conditionallyDuplicateOverloadsCausedByOverloadResolution.ts")]
#[case("conflictMarkerDiff3Trivia1.ts")]
#[case("conflictMarkerDiff3Trivia2.ts")]
#[case("conflictMarkerTrivia1.ts")]
#[case("conflictMarkerTrivia2.ts")]
#[case("conflictMarkerTrivia3.tsx")]
#[case("conflictMarkerTrivia4.ts")]
#[case("conflictingMemberTypesInBases.ts")]
#[case("conflictingTypeAnnotatedVar.ts")]
#[case("consistentAliasVsNonAliasRecordBehavior.ts")]
#[case("constDeclarationShadowedByVarDeclaration.ts")]
#[case("constDeclarationShadowedByVarDeclaration2.ts")]
#[case("constDeclarationShadowedByVarDeclaration3.ts")]
#[case("constDeclarations-access.ts")] // NOT RUNNABLE
#[case("constDeclarations-access2.ts")]
#[case("constDeclarations-access3.ts")]
#[case("constDeclarations-access4.ts")]
#[case("constDeclarations-access5.ts")] // NOT RUNNABLE
#[case("constDeclarations-ambient-errors.ts")]
#[case("constDeclarations-ambient.ts")]
#[case("constDeclarations-errors.ts")]
#[case("constDeclarations-es5.ts")]
#[case("constDeclarations-invalidContexts.ts")]
#[case("constDeclarations-scopes.ts")]
#[case("constDeclarations-scopes2.ts")]
#[case("constDeclarations-useBeforeDefinition.ts")]
#[case("constDeclarations-useBeforeDefinition2.ts")] // NOT RUNNABLE
#[case("constDeclarations-validContexts.ts")]
#[case("constDeclarations.ts")]
#[case("constDeclarations2.ts")]
#[case("constEnumBadPropertyNames.ts")]
#[case("constEnumDeclarations.ts")] // FAILING 999 on Number not implemented stuff
#[case("constEnumErrors.ts")] // FAILING 1000 on Number stuff
#[case("constEnumExternalModule.ts")] // NOT RUNNABLE
#[case("constEnumMergingWithValues1.ts")] // NOT RUNNABLE
#[case("constEnumMergingWithValues2.ts")] // NOT RUNNABLE
#[case("constEnumMergingWithValues3.ts")] // NOT RUNNABLE
#[case("constEnumMergingWithValues4.ts")] // NOT RUNNABLE
#[case("constEnumMergingWithValues5.ts")] // NOT RUNNABLE
#[case("constEnumNamespaceReferenceCausesNoImport.ts")] // NOT RUNNABLE
#[case("constEnumNamespaceReferenceCausesNoImport2.ts")] // NOT RUNNABLE
#[case("constEnumNoEmitReexport.ts")] // NOT RUNNABLE
#[case("constEnumNoPreserveDeclarationReexport.ts")] // NOT RUNNABLE
#[case("constEnumOnlyModuleMerging.ts")]
#[case("constEnumPreserveEmitNamedExport1.ts")] // NOT RUNNABLE
#[case("constEnumPreserveEmitNamedExport2.ts")] // NOT RUNNABLE
#[case("constEnumPreserveEmitReexport.ts")] // NOT RUNNABLE
#[case("constEnumSyntheticNodesComments.ts")]
#[case("constEnumToStringNoComments.ts")] // FAILING 1016 on Number stuff
#[case("constEnumToStringWithComments.ts")] // FAILING 1017 on Number stuff
#[case("constEnums.ts")] // FAILING 1018 on Number stuff
#[case("constInClassExpression.ts")]
#[case("constIndexedAccess.ts")]
#[case("constWithNonNull.ts")]
#[case("constantEnumAssert.ts")]
#[case("constantOverloadFunction.ts")]
#[case("constantOverloadFunctionNoSubtypeError.ts")]
#[case("constraintCheckInGenericBaseTypeReference.ts")]
#[case("constraintErrors1.ts")]
#[case("constraintOfRecursivelyMappedTypeWithConditionalIsResolvable.ts")]
#[case("constraintPropagationThroughReturnTypes.ts")]
#[case("constraintReferencingTypeParameterFromSameTypeParameterList.ts")]
#[case("constraints0.ts")]
#[case("constraintsThatReferenceOtherContstraints1.ts")]
#[case("constraintsUsedInPrototypeProperty.ts")]
#[case("constructorArgWithGenericCallSignature.ts")]
#[case("constructorArgs.ts")]
#[case("constructorArgsErrors1.ts")]
#[case("constructorArgsErrors2.ts")]
#[case("constructorArgsErrors3.ts")]
#[case("constructorArgsErrors4.ts")]
#[case("constructorArgsErrors5.ts")]
#[case("constructorAsType.ts")]
#[case("constructorInvocationWithTooFewTypeArgs.ts")]
#[case("constructorOverloads1.ts")]
#[case("constructorOverloads2.ts")]
#[case("constructorOverloads3.ts")]
#[case("constructorOverloads4.ts")]
#[case("constructorOverloads5.ts")]
#[case("constructorOverloads6.ts")]
#[case("constructorOverloads7.ts")]
#[case("constructorOverloads8.ts")]
#[case("constructorParametersInVariableDeclarations.ts")]
#[case("constructorParametersThatShadowExternalNamesInVariableDeclarations.ts")]
#[case("constructorReturningAPrimitive.ts")]
#[case("constructorReturnsInvalidType.ts")]
#[case("constructorStaticParamName.ts")]
#[case("constructorStaticParamNameErrors.ts")]
#[case("constructorTypeWithTypeParameters.ts")]
#[case("constructorWithCapturedSuper.ts")]
#[case("constructorWithIncompleteTypeAnnotation.ts")]
#[case("constructorsWithSpecializedSignatures.ts")]
#[case("contextSensitiveReturnTypeInference.ts")]
#[case("contextualExpressionTypecheckingDoesntBlowStack.ts")]
#[case("contextualOverloadListFromArrayUnion.ts")] // NOT RUNNABLE
#[case("contextualOverloadListFromUnionWithPrimitiveNoImplicitAny.ts")]
#[case("contextualPropertyOfGenericMappedType.ts")]
#[case("contextualReturnTypeOfIIFE.ts")]
#[case("contextualSigInstantiationRestParams.ts")]
#[case("contextualSignatureInstantiation1.ts")]
#[case("contextualSignatureInstantiation2.ts")]
#[case("contextualSignatureInstantiation3.ts")]
#[case("contextualSignatureInstantiation4.ts")]
#[case("contextualSignatureInstantiationWithTypeParameterConstrainedToOuterTypeParameter.ts")]
#[case("contextualSignatureInstatiationContravariance.ts")]
#[case("contextualSignatureInstatiationCovariance.ts")]
#[case("contextualSignature_objectLiteralMethodMayReturnNever.ts")]
#[case("contextualTypeAny.ts")]
#[case("contextualTypeAppliedToVarArgs.ts")]
#[case("contextualTypeArrayReturnType.ts")]
#[case("contextualTypeForInitalizedVariablesFiltersUndefined.ts")]
#[case("contextualTypeIterableUnions.ts")]
#[case("contextualTypeLogicalOr.ts")]
#[case("contextualTypeObjectSpreadExpression.ts")]
#[case("contextualTypeOfIndexedAccessParameter.ts")]
#[case("contextualTypeShouldBeLiteral.ts")]
#[case("contextualTyping.ts")]
#[case("contextualTyping1.ts")]
#[case("contextualTyping10.ts")]
#[case("contextualTyping11.ts")]
#[case("contextualTyping12.ts")]
#[case("contextualTyping13.ts")]
#[case("contextualTyping14.ts")]
#[case("contextualTyping15.ts")]
#[case("contextualTyping16.ts")]
#[case("contextualTyping17.ts")]
#[case("contextualTyping18.ts")]
#[case("contextualTyping19.ts")]
#[case("contextualTyping2.ts")]
#[case("contextualTyping20.ts")]
#[case("contextualTyping21.ts")]
#[case("contextualTyping22.ts")]
#[case("contextualTyping23.ts")]
#[case("contextualTyping24.ts")]
#[case("contextualTyping25.ts")]
#[case("contextualTyping26.ts")]
#[case("contextualTyping27.ts")]
#[case("contextualTyping28.ts")]
#[case("contextualTyping29.ts")]
#[case("contextualTyping3.ts")]
#[case("contextualTyping30.ts")]
#[case("contextualTyping31.ts")]
#[case("contextualTyping32.ts")]
#[case("contextualTyping33.ts")]
#[case("contextualTyping34.ts")]
#[case("contextualTyping35.ts")]
#[case("contextualTyping36.ts")]
#[case("contextualTyping37.ts")]
#[case("contextualTyping38.ts")]
#[case("contextualTyping39.ts")]
#[case("contextualTyping4.ts")]
#[case("contextualTyping40.ts")]
#[case("contextualTyping41.ts")]
#[case("contextualTyping5.ts")]
#[case("contextualTyping6.ts")]
#[case("contextualTyping7.ts")]
#[case("contextualTyping8.ts")]
#[case("contextualTyping9.ts")]
#[case("contextualTypingArrayDestructuringWithDefaults.ts")]
#[case("contextualTypingArrayOfLambdas.ts")]
#[case("contextualTypingFunctionReturningFunction.ts")]
#[case("contextualTypingFunctionReturningFunction2.ts")]
#[case("contextualTypingOfAccessors.ts")]
#[case("contextualTypingOfArrayLiterals1.ts")]
#[case("contextualTypingOfConditionalExpression.ts")]
#[case("contextualTypingOfConditionalExpression2.ts")]
#[case("contextualTypingOfGenericFunctionTypedArguments1.ts")]
#[case("contextualTypingOfLambdaReturnExpression.ts")]
#[case("contextualTypingOfLambdaWithMultipleSignatures.ts")]
#[case("contextualTypingOfLambdaWithMultipleSignatures2.ts")]
#[case("contextualTypingOfObjectLiterals.ts")]
#[case("contextualTypingOfObjectLiterals2.ts")]
#[case("contextualTypingOfOptionalMembers.tsx")] // NOT RUNNABLE
#[case("contextualTypingOfTooShortOverloads.ts")]
#[case("contextualTypingTwoInstancesOfSameTypeParameter.ts")]
#[case("contextualTypingWithFixedTypeParameters1.ts")] // FAILING 1143
#[case("contextualTypingWithGenericAndNonGenericSignature.ts")]
#[case("contextualTypingWithGenericSignature.ts")]
#[case("contextuallyTypeArgumentsKeyword.ts")] // NOT RUNNABLE
#[case("contextuallyTypedByDiscriminableUnion.ts")]
#[case("contextuallyTypedGenericAssignment.ts")]
#[case("contextuallyTypedParametersWithInitializers.ts")]
#[case("contextuallyTypingOrOperator.ts")]
#[case("contextuallyTypingOrOperator2.ts")]
#[case("contextuallyTypingOrOperator3.ts")]
#[case("contextuallyTypingRestParameters.ts")]
#[case("continueInIterationStatement1.ts")]
#[case("continueInIterationStatement2.ts")]
#[case("continueInIterationStatement3.ts")]
#[case("continueInIterationStatement4.ts")]
#[case("continueInLoopsWithCapturedBlockScopedBindings1.ts")]
#[case("continueLabel.ts")]
#[case("continueNotInIterationStatement1.ts")]
#[case("continueNotInIterationStatement2.ts")]
#[case("continueNotInIterationStatement3.ts")]
#[case("continueNotInIterationStatement4.ts")]
#[case("continueStatementInternalComments.ts")]
#[case("continueTarget1.ts")]
#[case("continueTarget2.ts")]
#[case("continueTarget3.ts")]
#[case("continueTarget4.ts")]
#[case("continueTarget5.ts")]
#[case("continueTarget6.ts")]
#[case("contravariantInferenceAndTypeGuard.ts")]
#[case("contravariantTypeAliasInference.ts")]
#[case("controlFlowAnalysisOnBareThisKeyword.ts")]
#[case("controlFlowArrayErrors.ts")]
#[case("controlFlowArrays.ts")]
#[case("controlFlowBreakContinueWithLabel.ts")]
#[case("controlFlowCaching.ts")]
#[case("controlFlowCommaExpressionAssertionWithinTernary.ts")]
#[case("controlFlowCommaExpressionFunctionCall.ts")]
#[case("controlFlowDestructuringLoop.ts")]
#[case("controlFlowDestructuringParameters.ts")]
#[case("controlFlowFinallyNoCatchAssignments.ts")]
#[case("controlFlowForCatchAndFinally.ts")]
#[case("controlFlowForCompoundAssignmentToThisMember.ts")]
#[case("controlFlowInstanceof.ts")] // NOT RUNNABLE
#[case("controlFlowJavascript.ts")] // NOT RUNNABLE
#[case("controlFlowLoopAnalysis.ts")]
#[case("controlFlowManyCallExpressionStatementsPerf.ts")]
#[case("controlFlowManyConsecutiveConditionsNoTimeout.ts")]
#[case("controlFlowNoImplicitAny.ts")]
#[case("controlFlowNullTypeAndLiteral.ts")]
#[case("controlFlowOuterVariable.ts")]
#[case("controlFlowPrivateClassField.ts")]
#[case("controlFlowPropertyDeclarations.ts")]
#[case("controlFlowPropertyInitializer.ts")]
#[case("controlFlowSelfReferentialLoop.ts")]
#[case("controlFlowWithIncompleteTypes.ts")]
#[case("convertClassExpressionToFunctionFromObjectProperty1.ts")]
#[case("convertClassExpressionToFunctionFromObjectProperty2.ts")]
#[case("convertKeywords.ts")]
#[case("convertKeywordsYes.ts")]
#[case("copyrightWithNewLine1.ts")]
#[case("copyrightWithoutNewLine1.ts")]
#[case("correctOrderOfPromiseMethod.ts")]
#[case("couldNotSelectGenericOverload.ts")]
#[case("covariance1.ts")]
#[case("crashInEmitTokenWithComment.ts")] // NOT RUNNABLE
#[case("crashInGetTextOfComputedPropertyName.ts")]
#[case("crashInResolveInterface.ts")] // NOT RUNNABLE
#[case("crashInresolveReturnStatement.ts")]
#[case("crashInsourcePropertyIsRelatableToTargetProperty.ts")]
#[case("crashIntypeCheckInvocationExpression.ts")]
#[case("crashIntypeCheckObjectCreationExpression.ts")]
#[case("crashOnMethodSignatures.ts")]
#[case("crashRegressionTest.ts")]
#[case("createArray.ts")]
#[case("curiousNestedConditionalEvaluationResult.ts")]
#[case("customAsyncIterator.ts")]
#[case("customEventDetail.ts")]
#[case("cyclicGenericTypeInstantiation.ts")]
#[case("cyclicGenericTypeInstantiationInference.ts")]
#[case("cyclicModuleImport.ts")]
#[case("cyclicTypeInstantiation.ts")]
#[case("debugger.ts")]
#[case("debuggerEmit.ts")]
#[case("declFileAccessors.ts")] // NOT RUNNABLE
#[case("declFileAliasUseBeforeDeclaration.ts")] // NOT RUNNABLE
#[case("declFileAliasUseBeforeDeclaration2.ts")]
#[case("declFileAmbientExternalModuleWithSingleExportedModule.ts")] // NOT RUNNABLE
#[case("declFileCallSignatures.ts")] // NOT RUNNABLE
#[case("declFileClassExtendsNull.ts")]
#[case("declFileClassWithIndexSignature.ts")]
#[case("declFileClassWithStaticMethodReturningConstructor.ts")]
#[case("declFileConstructSignatures.ts")] // NOT RUNNABLE
#[case("declFileConstructors.ts")] // NOT RUNNABLE
#[case("declFileEmitDeclarationOnly.ts")] // NOT RUNNABLE
#[case("declFileEmitDeclarationOnlyError1.ts")] // NOT RUNNABLE
#[case("declFileEmitDeclarationOnlyError2.ts")] // NOT RUNNABLE
#[case("declFileEnumUsedAsValue.ts")]
#[case("declFileEnums.ts")]
#[case("declFileExportAssignmentImportInternalModule.ts")]
#[case("declFileExportAssignmentOfGenericInterface.ts")] // NOT RUNNABLE
#[case("declFileExportImportChain.ts")] // NOT RUNNABLE
#[case("declFileExportImportChain2.ts")] // NOT RUNNABLE
#[case("declFileForClassWithMultipleBaseClasses.ts")]
#[case("declFileForClassWithPrivateOverloadedFunction.ts")]
#[case("declFileForExportedImport.ts")] // NOT RUNNABLE
#[case("declFileForFunctionTypeAsTypeParameter.ts")]
#[case("declFileForInterfaceWithOptionalFunction.ts")]
#[case("declFileForInterfaceWithRestParams.ts")]
#[case("declFileForTypeParameters.ts")]
#[case("declFileForVarList.ts")]
#[case("declFileFunctions.ts")] // NOT RUNNABLE
#[case("declFileGenericClassWithGenericExtendedClass.ts")]
#[case("declFileGenericType.ts")]
#[case("declFileGenericType2.ts")]
#[case("declFileImportChainInExportAssignment.ts")]
#[case("declFileImportModuleWithExportAssignment.ts")] // NOT RUNNABLE
#[case("declFileImportedTypeUseInTypeArgPosition.ts")]
#[case("declFileIndexSignatures.ts")] // NOT RUNNABLE
#[case("declFileInternalAliases.ts")]
#[case("declFileMethods.ts")] // NOT RUNNABLE
#[case("declFileModuleAssignmentInObjectLiteralProperty.ts")]
#[case("declFileModuleContinuation.ts")]
#[case("declFileModuleWithPropertyOfTypeModule.ts")]
#[case("declFileObjectLiteralWithAccessors.ts")]
#[case("declFileObjectLiteralWithOnlyGetter.ts")]
#[case("declFileObjectLiteralWithOnlySetter.ts")]
#[case("declFileOptionalInterfaceMethod.ts")]
#[case("declFilePrivateMethodOverloads.ts")]
#[case("declFilePrivateStatic.ts")]
#[case("declFileRegressionTests.ts")]
#[case("declFileRestParametersOfFunctionAndFunctionType.ts")]
#[case("declFileTypeAnnotationArrayType.ts")]
#[case("declFileTypeAnnotationBuiltInType.ts")]
#[case("declFileTypeAnnotationParenType.ts")]
#[case("declFileTypeAnnotationStringLiteral.ts")]
#[case("declFileTypeAnnotationTupleType.ts")]
#[case("declFileTypeAnnotationTypeAlias.ts")]
#[case("declFileTypeAnnotationTypeLiteral.ts")]
#[case("declFileTypeAnnotationTypeQuery.ts")]
#[case("declFileTypeAnnotationTypeReference.ts")]
#[case("declFileTypeAnnotationUnionType.ts")]
#[case("declFileTypeAnnotationVisibilityErrorAccessors.ts")]
#[case("declFileTypeAnnotationVisibilityErrorParameterOfFunction.ts")]
#[case("declFileTypeAnnotationVisibilityErrorReturnTypeOfFunction.ts")]
#[case("declFileTypeAnnotationVisibilityErrorTypeAlias.ts")]
#[case("declFileTypeAnnotationVisibilityErrorTypeLiteral.ts")]
#[case("declFileTypeAnnotationVisibilityErrorVariableDeclaration.ts")]
#[case("declFileTypeofClass.ts")]
#[case("declFileTypeofEnum.ts")]
#[case("declFileTypeofFunction.ts")]
#[case("declFileTypeofInAnonymousType.ts")]
#[case("declFileTypeofModule.ts")]
#[case("declFileWithClassNameConflictingWithClassReferredByExtendsClause.ts")]
#[case("declFileWithErrorsInInputDeclarationFile.ts")] // NOT RUNNABLE
#[case("declFileWithErrorsInInputDeclarationFileWithOut.ts")] // NOT RUNNABLE
#[case("declFileWithExtendsClauseThatHasItsContainerNameConflict.ts")]
#[case("declFileWithInternalModuleNameConflictsInExtendsClause1.ts")]
#[case("declFileWithInternalModuleNameConflictsInExtendsClause2.ts")]
#[case("declFileWithInternalModuleNameConflictsInExtendsClause3.ts")]
#[case("declInput-2.ts")]
#[case("declInput.ts")]
#[case("declInput3.ts")]
#[case("declInput4.ts")]
#[case("declarationEmitAliasExportStar.ts")] // NOT RUNNABLE
#[case("declarationEmitAliasFromIndirectFile.ts")] // NOT RUNNABLE
#[case("declarationEmitAmdModuleDefault.ts")]
#[case("declarationEmitAmdModuleNameDirective.ts")] // NOT RUNNABLE
#[case("declarationEmitArrayTypesFromGenericArrayUsage.ts")]
#[case("declarationEmitBindingPatterns.ts")]
#[case("declarationEmitBundlePreservesHasNoDefaultLibDirective.ts")] // NOT RUNNABLE
#[case("declarationEmitBundleWithAmbientReferences.ts")] // NOT RUNNABLE
#[case("declarationEmitClassMemberNameConflict.ts")]
#[case("declarationEmitClassMemberNameConflict2.ts")]
#[case("declarationEmitClassPrivateConstructor.ts")]
#[case("declarationEmitClassPrivateConstructor2.ts")]
#[case("declarationEmitCommonJsModuleReferencedType.ts")] // NOT RUNNABLE
#[case("declarationEmitCommonSourceDirectoryDoesNotContainAllFiles.ts")] // NOT RUNNABLE
#[case("declarationEmitComputedNameCausesImportToBePainted.ts")] // NOT RUNNABLE
#[case("declarationEmitComputedNameConstEnumAlias.ts")] // NOT RUNNABLE
#[case("declarationEmitConstantNoWidening.ts")]
#[case("declarationEmitCrossFileImportTypeOfAmbientModule.ts")] // NOT RUNNABLE
#[case("declarationEmitDefaultExport1.ts")]
#[case("declarationEmitDefaultExport2.ts")]
#[case("declarationEmitDefaultExport3.ts")]
#[case("declarationEmitDefaultExport4.ts")]
#[case("declarationEmitDefaultExport5.ts")]
#[case("declarationEmitDefaultExport6.ts")]
#[case("declarationEmitDefaultExport7.ts")]
#[case("declarationEmitDefaultExport8.ts")]
#[case("declarationEmitDefaultExportWithStaticAssignment.ts")] // NOT RUNNABLE
#[case("declarationEmitDefaultExportWithTempVarName.ts")] // NOT RUNNABLE
#[case("declarationEmitDefaultExportWithTempVarNameWithBundling.ts")] // NOT RUNNABLE
#[case("declarationEmitDestructuring1.ts")]
#[case("declarationEmitDestructuring2.ts")]
#[case("declarationEmitDestructuring3.ts")]
#[case("declarationEmitDestructuring4.ts")]
#[case("declarationEmitDestructuring5.ts")]
#[case("declarationEmitDestructuringArrayPattern1.ts")]
#[case("declarationEmitDestructuringArrayPattern2.ts")]
#[case("declarationEmitDestructuringArrayPattern3.ts")]
#[case("declarationEmitDestructuringArrayPattern4.ts")]
#[case("declarationEmitDestructuringArrayPattern5.ts")]
#[case("declarationEmitDestructuringObjectLiteralPattern.ts")]
#[case("declarationEmitDestructuringObjectLiteralPattern1.ts")]
#[case("declarationEmitDestructuringObjectLiteralPattern2.ts")]
#[case("declarationEmitDestructuringOptionalBindingParametersInOverloads.ts")]
#[case("declarationEmitDestructuringParameterProperties.ts")]
#[case("declarationEmitDestructuringPrivacyError.ts")]
#[case("declarationEmitDestructuringWithOptionalBindingParameters.ts")]
#[case("declarationEmitDetachedComment1.ts")] // NOT RUNNABLE
#[case("declarationEmitDetachedComment2.ts")] // NOT RUNNABLE
#[case("declarationEmitDoesNotUseReexportedNamespaceAsLocal.ts")] // NOT RUNNABLE
#[case("declarationEmitEnumReadonlyProperty.ts")]
#[case("declarationEmitExpandoPropertyPrivateName.ts")] // NOT RUNNABLE
#[case("declarationEmitExpandoWithGenericConstraint.ts")]
#[case("declarationEmitExportAliasVisibiilityMarking.ts")] // NOT RUNNABLE
#[case("declarationEmitExportAssignedNamespaceNoTripleSlashTypesReference.ts")] // NOT RUNNABLE
#[case("declarationEmitExportAssignment.ts")] // NOT RUNNABLE
#[case("declarationEmitExportDeclaration.ts")] // NOT RUNNABLE
#[case("declarationEmitExpressionInExtends.ts")]
#[case("declarationEmitExpressionInExtends2.ts")]
#[case("declarationEmitExpressionInExtends3.ts")]
#[case("declarationEmitExpressionInExtends4.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitExpressionInExtends5.ts")]
#[case("declarationEmitExpressionInExtends6.ts")] // NOT RUNNABLE
#[case("declarationEmitExpressionInExtends7.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitExpressionWithNonlocalPrivateUniqueSymbol.ts")] // NOT RUNNABLE
#[case("declarationEmitFBoundedTypeParams.ts")]
#[case("declarationEmitFirstTypeArgumentGenericFunctionType.ts")]
#[case("declarationEmitForDefaultExportClassExtendingExpression01.ts")]
#[case("declarationEmitForGlobalishSpecifierSymlink.ts")] // NOT RUNNABLE
#[case("declarationEmitForGlobalishSpecifierSymlink2.ts")] // NOT RUNNABLE
#[case("declarationEmitForModuleImportingModuleAugmentationRetainsImport.ts")] // NOT RUNNABLE
#[case("declarationEmitForTypesWhichNeedImportTypes.ts")] // NOT RUNNABLE
#[case("declarationEmitFunctionDuplicateNamespace.ts")]
#[case("declarationEmitFunctionKeywordProp.ts")]
#[case("declarationEmitHasTypesRefOnNamespaceUse.ts")] // NOT RUNNABLE
#[case("declarationEmitImportInExportAssignmentModule.ts")]
#[case("declarationEmitIndexTypeArray.ts")]
#[case("declarationEmitIndexTypeNotFound.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitInferredDefaultExportType.ts")]
#[case("declarationEmitInferredDefaultExportType2.ts")]
#[case("declarationEmitInferredTypeAlias1.ts")] // NOT RUNNABLE
#[case("declarationEmitInferredTypeAlias2.ts")] // NOT RUNNABLE
#[case("declarationEmitInferredTypeAlias3.ts")] // NOT RUNNABLE
#[case("declarationEmitInferredTypeAlias4.ts")]
#[case("declarationEmitInferredTypeAlias5.ts")] // NOT RUNNABLE
#[case("declarationEmitInferredTypeAlias6.ts")] // NOT RUNNABLE
#[case("declarationEmitInferredTypeAlias7.ts")] // NOT RUNNABLE
#[case("declarationEmitInferredTypeAlias8.ts")]
#[case("declarationEmitInferredTypeAlias9.ts")]
#[case("declarationEmitInterfaceWithNonEntityNameExpressionHeritage.ts")]
#[case("declarationEmitInvalidExport.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitInvalidReference.ts")]
#[case("declarationEmitInvalidReference2.ts")]
#[case("declarationEmitInvalidReferenceAllowJs.ts")]
#[case("declarationEmitLambdaWithMissingTypeParameterNoCrash.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitLocalClassDeclarationMixin.ts")]
#[case("declarationEmitLocalClassHasRequiredDeclare.ts")]
#[case("declarationEmitMappedPrivateTypeTypeParameter.ts")] // NOT RUNNABLE
#[case("declarationEmitMixinPrivateProtected.ts")] // NOT RUNNABLE
#[case("declarationEmitModuleWithScopeMarker.ts")]
#[case("declarationEmitNameConflicts.ts")] // NOT RUNNABLE
#[case("declarationEmitNameConflicts2.ts")]
#[case("declarationEmitNameConflicts3.ts")]
#[case("declarationEmitNameConflictsWithAlias.ts")]
#[case("declarationEmitNestedGenerics.ts")]
#[case("declarationEmitNoNonRequiredParens.ts")]
#[case("declarationEmitObjectAssignedDefaultExport.ts")] // NOT RUNNABLE
#[case("declarationEmitOfFuncspace.ts")] // NOT RUNNABLE
#[case("declarationEmitOfTypeofAliasedExport.ts")] // NOT RUNNABLE
#[case("declarationEmitOptionalMethod.ts")]
#[case("declarationEmitOutFileBundlePaths.ts")] // NOT RUNNABLE
#[case("declarationEmitOverloadedPrivateInference.ts")]
#[case("declarationEmitParameterProperty.ts")]
#[case("declarationEmitPathMappingMonorepo.ts")] // NOT RUNNABLE
#[case("declarationEmitPathMappingMonorepo2.ts")] // NOT RUNNABLE
#[case("declarationEmitPrefersPathKindBasedOnBundling.ts")] // NOT RUNNABLE
#[case("declarationEmitPrefersPathKindBasedOnBundling2.ts")] // NOT RUNNABLE
#[case("declarationEmitPreservesHasNoDefaultLibDirective.ts")]
#[case("declarationEmitPrivateAsync.ts")]
#[case("declarationEmitPrivateNameCausesError.ts")] // NOT RUNNABLE
#[case("declarationEmitPrivatePromiseLikeInterface.ts")] // NOT RUNNABLE
#[case("declarationEmitPrivateReadonlyLiterals.ts")]
#[case("declarationEmitPrivateSymbolCausesVarDeclarationEmit2.ts")] // NOT RUNNABLE
#[case("declarationEmitPrivateSymbolCausesVarDeclarationToBeEmitted.ts")]
#[case("declarationEmitPromise.ts")]
#[case("declarationEmitProtectedMembers.ts")]
#[case("declarationEmitQualifiedAliasTypeArgument.ts")] // NOT RUNNABLE
#[case("declarationEmitReadonlyComputedProperty.ts")] // NOT RUNNABLE
#[case("declarationEmitReexportedSymlinkReference.ts")] // NOT RUNNABLE
#[case("declarationEmitReexportedSymlinkReference2.ts")] // NOT RUNNABLE
#[case("declarationEmitReexportedSymlinkReference3.ts")] // NOT RUNNABLE
#[case("declarationEmitRelativeModuleError.ts")]
#[case("declarationEmitRetainsJsdocyComments.ts")]
#[case("declarationEmitShadowingInferNotRenamed.ts")]
#[case("declarationEmitSpreadStringlyKeyedEnum.ts")]
#[case("declarationEmitStringEnumUsedInNonlocalSpread.ts")] // NOT RUNNABLE
#[case("declarationEmitSymlinkPaths.ts")] // NOT RUNNABLE
#[case("declarationEmitToDeclarationDirWithCompositeOption.ts")] // NOT RUNNABLE
#[case("declarationEmitToDeclarationDirWithDeclarationOption.ts")] // NOT RUNNABLE
#[case("declarationEmitToDeclarationDirWithoutCompositeAndDeclarationOptions.ts")] // NOT RUNNABLE
#[case("declarationEmitTupleRestSignatureLeadingVariadic.ts")]
#[case("declarationEmitTypeAliasTypeParameterExtendingUnknownSymbol.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitTypeAliasWithTypeParameters1.ts")] // NOT RUNNABLE
#[case("declarationEmitTypeAliasWithTypeParameters2.ts")] // NOT RUNNABLE
#[case("declarationEmitTypeAliasWithTypeParameters3.ts")]
#[case("declarationEmitTypeAliasWithTypeParameters4.ts")]
#[case("declarationEmitTypeAliasWithTypeParameters5.ts")]
#[case("declarationEmitTypeAliasWithTypeParameters6.ts")]
#[case("declarationEmitTypeParamMergedWithPrivate.ts")]
#[case("declarationEmitTypeofDefaultExport.ts")] // NOT RUNNABLE
#[case("declarationEmitUnknownImport.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitUnknownImport2.ts")] // OUT OF SCOPE transformation error
#[case("declarationEmitUnnessesaryTypeReferenceNotAdded.ts")] // NOT RUNNABLE
#[case("declarationEmitWithComposite.ts")] // NOT RUNNABLE
#[case("declarationEmitWithDefaultAsComputedName.ts")] // NOT RUNNABLE
#[case("declarationEmitWithDefaultAsComputedName2.ts")] // NOT RUNNABLE
#[case("declarationEmitWithInvalidPackageJsonTypings.ts")] // NOT RUNNABLE
#[case("declarationFileNoCrashOnExtraExportModifier.ts")] // NOT RUNNABLE
#[case("declarationFileOverwriteError.ts")] // NOT RUNNABLE
#[case("declarationFileOverwriteErrorWithOut.ts")] // NOT RUNNABLE
#[case("declarationFilesGeneratingTypeReferences.ts")] // NOT RUNNABLE
#[case("declarationFilesWithTypeReferences1.ts")] // NOT RUNNABLE
#[case("declarationFilesWithTypeReferences2.ts")] // NOT RUNNABLE
#[case("declarationFilesWithTypeReferences3.ts")] // NOT RUNNABLE
#[case("declarationFilesWithTypeReferences4.ts")] // NOT RUNNABLE
#[case("declarationFunctionTypeNonlocalShouldNotBeAnError.ts")]
#[case("declarationImportTypeAliasInferredAndEmittable.ts")] // NOT RUNNABLE
#[case("declarationMaps.ts")] // NOT RUNNABLE
#[case("declarationMapsMultifile.ts")] // NOT RUNNABLE
#[case("declarationMapsOutFile.ts")] // NOT RUNNABLE
#[case("declarationMapsOutFile2.ts")] // NOT RUNNABLE
#[case("declarationMapsWithSourceMap.ts")] // NOT RUNNABLE
#[case("declarationMapsWithoutDeclaration.ts")] // NOT RUNNABLE
#[case("declarationMerging1.ts")] // NOT RUNNABLE
#[case("declarationMerging2.ts")] // NOT RUNNABLE
#[case("declarationNoDanglingGenerics.ts")]
#[case("declarationQuotedMembers.ts")]
#[case("declarationTypecheckNoUseBeforeReferenceCheck.ts")] // NOT RUNNABLE
#[case("declarationsForFileShadowingGlobalNoError.ts")] // NOT RUNNABLE
#[case("declarationsForIndirectTypeAliasReference.ts")] // NOT RUNNABLE
#[case("declarationsForInferredTypeFromOtherFile.ts")] // NOT RUNNABLE
#[case("declarationsIndirectGeneratedAliasReference.ts")] // NOT RUNNABLE
#[case("declarationsWithRecursiveInternalTypesProduceUniqueTypeParams.ts")]
#[case("declareAlreadySeen.ts")]
#[case("declareClassInterfaceImplementation.ts")]
#[case("declareDottedExtend.ts")]
#[case("declareDottedModuleName.ts")]
#[case("declareExternalModuleWithExportAssignedFundule.ts")]
#[case("declareFileExportAssignment.ts")]
#[case("declareFileExportAssignmentWithVarFromVariableStatement.ts")]
#[case("declareIdentifierAsBeginningOfStatementExpression01.ts")]
#[case("declareModifierOnImport1.ts")]
#[case("declareModifierOnTypeAlias.ts")]
#[case("declaredExternalModule.ts")]
#[case("declaredExternalModuleWithExportAssignment.ts")]
#[case("decoratorInJsFile.ts")] // NOT RUNNABLE
#[case("decoratorInJsFile1.ts")] // NOT RUNNABLE
#[case("decoratorMetadataConditionalType.ts")]
#[case("decoratorMetadataForMethodWithNoReturnTypeAnnotation01.ts")]
#[case("decoratorMetadataGenericTypeVariable.ts")]
#[case("decoratorMetadataGenericTypeVariableDefault.ts")]
#[case("decoratorMetadataGenericTypeVariableInScope.ts")]
#[case("decoratorMetadataNoLibIsolatedModulesTypes.ts")]
#[case("decoratorMetadataNoStrictNull.ts")]
#[case("decoratorMetadataOnInferredType.ts")]
#[case("decoratorMetadataPromise.ts")]
#[case("decoratorMetadataRestParameterWithImportedType.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithConstructorType.ts")]
#[case("decoratorMetadataWithImportDeclarationNameCollision.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision2.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision3.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision4.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision5.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision6.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision7.ts")] // NOT RUNNABLE
#[case("decoratorMetadataWithImportDeclarationNameCollision8.ts")] // NOT RUNNABLE
#[case("decoratorReferenceOnOtherProperty.ts")] // NOT RUNNABLE
#[case("decoratorReferences.ts")]
#[case("decoratorWithNegativeLiteralTypeNoCrash.ts")]
#[case("decoratorWithUnderscoreMethod.ts")]
#[case("decoratorsOnComputedProperties.ts")]
#[case("decrementAndIncrementOperators.ts")]
#[case("deduplicateImportsInSystem.ts")]
#[case("deepComparisons.ts")]
#[case("deepElaborationsIntoArrowExpressions.ts")]
#[case("deepExcessPropertyCheckingWhenTargetIsIntersection.ts")]
#[case("deepKeysIndexing.ts")]
#[case("deeplyDependentLargeArrayMutation.ts")] // NOT RUNNABLE
#[case("deeplyDependentLargeArrayMutation2.ts")] // NOT RUNNABLE
#[case("deeplyNestedAssignabilityErrorsCombined.ts")]
#[case("deeplyNestedAssignabilityIssue.ts")] // FAILING 1535 on pretty-printing (looks right just not correctly massaging line numbers)
#[case("deeplyNestedCheck.ts")]
#[case("deeplyNestedConditionalTypes.ts")]
#[case("deeplyNestedConstraints.ts")]
#[case("defaultArgsInFunctionExpressions.ts")]
#[case("defaultArgsInOverloads.ts")]
#[case("defaultBestCommonTypesHaveDecls.ts")]
#[case("defaultDeclarationEmitDefaultImport.ts")] // NOT RUNNABLE
#[case("defaultDeclarationEmitNamedCorrectly.ts")]
#[case("defaultDeclarationEmitShadowedNamedCorrectly.ts")] // NOT RUNNABLE
#[case("defaultIndexProps1.ts")]
#[case("defaultIndexProps2.ts")]
#[case("defaultIsNotVisibleInLocalScope.ts")] // NOT RUNNABLE
#[case("defaultKeywordWithoutExport1.ts")]
#[case("defaultKeywordWithoutExport2.ts")]
#[case("defaultOfAnyInStrictNullChecks.ts")]
#[case("defaultParameterAddsUndefinedWithStrictNullChecks.ts")]
#[case("defaultParameterTrailingComments.ts")]
#[case("defaultPropsEmptyCurlyBecomesAnyForJs.ts")] // NOT RUNNABLE
#[case("defaultValueInConstructorOverload1.ts")]
#[case("defaultValueInFunctionOverload1.ts")]
#[case("defaultValueInFunctionTypes.ts")]
#[case("deferredLookupTypeResolution.ts")]
#[case("deferredLookupTypeResolution2.ts")]
#[case("deferredTypeReferenceWithinArrayWithinTuple.ts")]
#[case("defineVariables_useDefineForClassFields.ts")]
#[case("definiteAssignmentOfDestructuredVariable.ts")]
#[case("definiteAssignmentWithErrorStillStripped.ts")]
#[case("deleteExpressionMustBeOptional.ts")] // NOT RUNNABLE
#[case("deleteExpressionMustBeOptional_exactOptionalPropertyTypes.ts")] // NOT RUNNABLE
#[case("deleteOperator1.ts")]
#[case("deleteOperatorInStrictMode.ts")]
#[case("deleteReadonly.ts")]
#[case("dependencyViaImportAlias.ts")] // NOT RUNNABLE
#[case("deprecatedBool.ts")]
#[case("derivedClassConstructorWithExplicitReturns01.ts")]
#[case("derivedClassOverridesPrivateFunction1.ts")]
#[case("derivedClasses.ts")]
#[case("derivedInterfaceCallSignature.ts")]
#[case("derivedTypeCallingBaseImplWithOptionalParams.ts")]
#[case("derivedTypeIncompatibleSignatures.ts")]
#[case("destructionAssignmentError.ts")]
#[case("destructureComputedProperty.ts")]
#[case("destructureOfVariableSameAsShorthand.ts")]
#[case("destructureOptionalParameter.ts")]
#[case("destructuredDeclarationEmit.ts")] // NOT RUNNABLE
#[case("destructuredLateBoundNameHasCorrectTypes.ts")]
#[case("destructuredMaappedTypeIsNotImplicitlyAny.ts")]
#[case("destructuringAssignmentWithDefault.ts")]
#[case("destructuringAssignmentWithExportedName.ts")]
#[case("destructuringAssignmentWithStrictNullChecks.ts")]
#[case("destructuringAssignment_private.ts")]
#[case("destructuringControlFlowNoCrash.ts")]
#[case("destructuringFromUnionSpread.ts")]
#[case("destructuringInVariableDeclarations1.ts")]
#[case("destructuringInVariableDeclarations2.ts")]
#[case("destructuringInVariableDeclarations3.ts")]
#[case("destructuringInVariableDeclarations4.ts")]
#[case("destructuringInVariableDeclarations5.ts")]
#[case("destructuringInVariableDeclarations6.ts")]
#[case("destructuringInVariableDeclarations7.ts")]
#[case("destructuringInVariableDeclarations8.ts")]
#[case("destructuringInitializerContextualTypeFromContext.ts")]
#[case("destructuringPropertyAssignmentNameIsNotAssignmentTarget.ts")]
#[case("destructuringTempOccursAfterPrologue.ts")]
#[case("destructuringTuple.ts")]
#[case("destructuringTypeGuardFlow.ts")]
#[case("destructuringWithConstraint.ts")]
#[case("destructuringWithGenericParameter.ts")]
#[case("destructuringWithNewExpression.ts")]
#[case("destructuringWithNumberLiteral.ts")]
#[case("detachedCommentAtStartOfConstructor1.ts")]
#[case("detachedCommentAtStartOfConstructor2.ts")]
#[case("detachedCommentAtStartOfFunctionBody1.ts")]
#[case("detachedCommentAtStartOfFunctionBody2.ts")]
#[case("detachedCommentAtStartOfLambdaFunction1.ts")]
#[case("detachedCommentAtStartOfLambdaFunction2.ts")]
#[case("didYouMeanElaborationsForExpressionsWhichCouldBeCalled.ts")]
#[case("didYouMeanStringLiteral.ts")] // FAILING 1613
#[case("didYouMeanSuggestionErrors.ts")]
#[case("differentTypesWithSameName.ts")]
#[case("discriminableUnionWithIntersectedMembers.ts")]
#[case("discriminantElementAccessCheck.ts")]
#[case("discriminantPropertyCheck.ts")] // FAILING 1618 on Number stuff
#[case("discriminantPropertyInference.ts")]
#[case("discriminantsAndNullOrUndefined.ts")]
#[case("discriminantsAndPrimitives.ts")]
#[case("discriminantsAndTypePredicates.ts")]
#[case("discriminateObjectTypesOnly.ts")]
#[case("discriminatedUnionErrorMessage.ts")]
#[case("discriminatedUnionJsxElement.tsx")]
#[case("divergentAccessors1.ts")]
#[case("divergentAccessorsTypes1.ts")]
#[case("divergentAccessorsTypes2.ts")]
#[case("divergentAccessorsVisibility1.ts")]
#[case("doNotElaborateAssignabilityToTypeParameters.ts")]
#[case("doNotEmitDetachedComments.ts")]
#[case("doNotEmitDetachedCommentsAtStartOfConstructor.ts")]
#[case("doNotEmitDetachedCommentsAtStartOfFunctionBody.ts")]
#[case("doNotEmitDetachedCommentsAtStartOfLambdaFunction.ts")]
#[case("doNotEmitPinnedCommentNotOnTopOfFile.ts")]
#[case("doNotEmitPinnedCommentOnNotEmittedNode.ts")] // NOT RUNNABLE
#[case("doNotEmitPinnedCommentOnNotEmittedNodets.ts")]
#[case("doNotEmitPinnedDetachedComments.ts")]
#[case("doNotEmitTripleSlashCommentsInEmptyFile.ts")] // NOT RUNNABLE
#[case("doNotEmitTripleSlashCommentsOnNotEmittedNode.ts")] // NOT RUNNABLE
#[case("doNotInferUnrelatedTypes.ts")]
#[case("doNotWidenAtObjectLiteralPropertyAssignment.ts")]
#[case("doNotemitTripleSlashComments.ts")] // NOT RUNNABLE
#[case("doWhileLoop.ts")]
#[case("doWhileUnreachableCode.ts")]
#[case("doYouNeedToChangeYourTargetLibraryES2015.ts")]
#[case("doYouNeedToChangeYourTargetLibraryES2016Plus.ts")]
#[case("doesNotNarrowUnionOfConstructorsWithInstanceof.ts")]
#[case("dontShowCompilerGeneratedMembers.ts")] // NOT RUNNABLE
#[case("dottedModuleName.ts")] // NOT RUNNABLE
#[case("dottedModuleName2.ts")]
#[case("dottedNamesInSystem.ts")]
#[case("dottedSymbolResolution1.ts")] // NOT RUNNABLE
#[case("doubleMixinConditionalTypeBaseClassWorks.ts")]
#[case("doubleUnderStringLiteralAssignability.ts")]
#[case("doubleUnderscoreEnumEmit.ts")]
#[case("doubleUnderscoreExportStarConflict.ts")] // NOT RUNNABLE
#[case("doubleUnderscoreLabels.ts")]
#[case("doubleUnderscoreMappedTypes.ts")]
#[case("doubleUnderscoreReactNamespace.ts")] // NOT RUNNABLE
#[case("downlevelLetConst1.ts")]
#[case("downlevelLetConst10.ts")] // NOT RUNNABLE
#[case("downlevelLetConst11.ts")]
#[case("downlevelLetConst12.ts")]
#[case("downlevelLetConst13.ts")]
#[case("downlevelLetConst14.ts")]
#[case("downlevelLetConst15.ts")]
#[case("downlevelLetConst16.ts")]
#[case("downlevelLetConst17.ts")]
#[case("downlevelLetConst18.ts")]
#[case("downlevelLetConst19.ts")]
#[case("downlevelLetConst2.ts")]
#[case("downlevelLetConst3.ts")]
#[case("downlevelLetConst4.ts")]
#[case("downlevelLetConst5.ts")]
#[case("downlevelLetConst6.ts")]
#[case("downlevelLetConst7.ts")]
#[case("downlevelLetConst8.ts")]
#[case("downlevelLetConst9.ts")]
#[case("duplicateAnonymousInners1.ts")]
#[case("duplicateAnonymousModuleClasses.ts")]
#[case("duplicateClassElements.ts")]
#[case("duplicateConstructSignature.ts")]
#[case("duplicateConstructSignature2.ts")]
#[case("duplicateConstructorOverloadSignature.ts")]
#[case("duplicateConstructorOverloadSignature2.ts")]
#[case("duplicateDefaultExport.ts")]
#[case("duplicateIdentifierBindingElementInParameterDeclaration1.ts")]
#[case("duplicateIdentifierBindingElementInParameterDeclaration2.ts")]
#[case("duplicateIdentifierComputedName.ts")]
#[case("duplicateIdentifierDifferentModifiers.ts")]
#[case("duplicateIdentifierDifferentSpelling.ts")]
#[case("duplicateIdentifierEnum.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierInCatchBlock.ts")]
#[case("duplicateIdentifierRelatedSpans1.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans2.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans3.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans4.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans5.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans6.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans7.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierRelatedSpans_moduleAugmentation.ts")] // NOT RUNNABLE
#[case("duplicateIdentifierShouldNotShortCircuitBaseTypeBinding.ts")] // NOT RUNNABLE
#[case("duplicateIdentifiersAcrossContainerBoundaries.ts")]
#[case("duplicateIdentifiersAcrossFileBoundaries.ts")] // NOT RUNNABLE
#[case("duplicateInterfaceMembers1.ts")]
#[case("duplicateLabel1.ts")]
#[case("duplicateLabel2.ts")]
#[case("duplicateLabel3.ts")]
#[case("duplicateLabel4.ts")]
#[case("duplicateLocalVariable1.ts")]
#[case("duplicateLocalVariable2.ts")]
#[case("duplicateLocalVariable3.ts")]
#[case("duplicateLocalVariable4.ts")]
#[case("duplicateObjectLiteralProperty.ts")]
#[case("duplicateObjectLiteralProperty_computedName.ts")]
#[case("duplicateOverloadInTypeAugmentation1.ts")]
#[case("duplicatePackage.ts")] // NOT RUNNABLE
#[case("duplicatePackage_globalMerge.ts")] // NOT RUNNABLE
#[case("duplicatePackage_packageIdIncludesSubModule.ts")] // NOT RUNNABLE
#[case("duplicatePackage_referenceTypes.ts")] // NOT RUNNABLE
#[case("duplicatePackage_relativeImportWithinPackage.ts")] // NOT RUNNABLE
#[case("duplicatePackage_relativeImportWithinPackage_scoped.ts")] // NOT RUNNABLE
#[case("duplicatePackage_subModule.ts")] // NOT RUNNABLE
#[case("duplicatePackage_withErrors.ts")] // NOT RUNNABLE
#[case("duplicatePropertiesInStrictMode.ts")]
#[case("duplicateStringNamedProperty1.ts")]
#[case("duplicateSymbolsExportMatching.ts")]
#[case("duplicateTypeParameters1.ts")]
#[case("duplicateTypeParameters2.ts")]
#[case("duplicateTypeParameters3.ts")]
#[case("duplicateVarAndImport.ts")]
#[case("duplicateVarAndImport2.ts")]
#[case("duplicateVariableDeclaration1.ts")]
#[case("duplicateVariablesByScope.ts")]
#[case("duplicateVariablesWithAny.ts")]
#[case("duplicateVarsAcrossFileBoundaries.ts")] // NOT RUNNABLE
#[case("dynamicImportInDefaultExportExpression.ts")]
#[case("dynamicImportTrailingComma.ts")]
#[case("dynamicImportWithNestedThis_es2015.ts")]
#[case("dynamicImportWithNestedThis_es5.ts")]
#[case("dynamicModuleTypecheckError.ts")]
#[case("dynamicNames.ts")] // NOT RUNNABLE
#[case("dynamicNamesErrors.ts")]
#[case("dynamicRequire.ts")] // NOT RUNNABLE
#[case("elaboratedErrors.ts")]
#[case("elaboratedErrorsOnNullableTargets01.ts")]
#[case("elaborationForPossiblyCallableTypeStillReferencesArgumentAtTopLevel.ts")]
#[case("elementAccessExpressionInternalComments.ts")]
#[case("elidedEmbeddedStatementsReplacedWithSemicolon.ts")]
#[case("elidingImportNames.ts")] // NOT RUNNABLE
#[case("emitAccessExpressionOfCastedObjectLiteralExpressionInArrowFunctionES5.ts")]
#[case("emitAccessExpressionOfCastedObjectLiteralExpressionInArrowFunctionES6.ts")]
#[case("emitBOM.ts")] // NOT RUNNABLE
#[case("emitBundleWithPrologueDirectives1.ts")] // NOT RUNNABLE
#[case("emitBundleWithShebang1.ts")] // NOT PASSABLE relies on manipulation of test file contents
#[case("emitBundleWithShebang2.ts")] // NOT RUNNABLE
#[case("emitBundleWithShebangAndPrologueDirectives1.ts")] // NOT RUNNABLE
#[case("emitBundleWithShebangAndPrologueDirectives2.ts")] // NOT RUNNABLE
#[case("emitCapturingThisInTupleDestructuring1.ts")]
#[case("emitCapturingThisInTupleDestructuring2.ts")]
#[case("emitClassExpressionInDeclarationFile.ts")]
#[case("emitClassExpressionInDeclarationFile2.ts")] // OUT OF SCOPE transformation error
#[case("emitClassMergedWithConstNamespaceNotElided.ts")] // NOT RUNNABLE
#[case("emitCommentsOnlyFile.ts")]
#[case("emitDecoratorMetadata_object.ts")]
#[case("emitDecoratorMetadata_restArgs.ts")]
#[case("emitHelpersWithLocalCollisions.ts")] // NOT RUNNABLE
#[case("emitMemberAccessExpression.ts")] // NOT RUNNABLE
#[case("emitPinnedCommentsOnTopOfFile.ts")]
#[case("emitPostComments.ts")]
#[case("emitPreComments.ts")]
#[case("emitSkipsThisWithRestParameter.ts")]
#[case("emitSuperCallBeforeEmitParameterPropertyDeclaration1.ts")]
#[case("emitSuperCallBeforeEmitParameterPropertyDeclaration1ES6.ts")]
#[case("emitSuperCallBeforeEmitPropertyDeclaration1.ts")]
#[case("emitSuperCallBeforeEmitPropertyDeclaration1ES6.ts")]
#[case("emitSuperCallBeforeEmitPropertyDeclarationAndParameterPropertyDeclaration1.ts")]
#[case("emitSuperCallBeforeEmitPropertyDeclarationAndParameterPropertyDeclaration1ES6.ts")]
#[case("emitThisInObjectLiteralGetter.ts")]
#[case("emitThisInSuperMethodCall.ts")]
#[case("emitTopOfFileTripleSlashCommentOnNotEmittedNodeIfRemoveCommentsIsFalse.ts")] // NOT RUNNABLE
#[case("emptyArgumentsListComment.ts")]
#[case("emptyArrayDestructuringExpressionVisitedByTransformer.ts")]
#[case("emptyDeclarationEmitIsModule.ts")] // NOT RUNNABLE
#[case("emptyEnum.ts")]
#[case("emptyExpr.ts")]
#[case("emptyFile-declaration.ts")]
#[case("emptyFile-souremap.ts")]
#[case("emptyFile.ts")]
#[case("emptyGenericParamList.ts")]
#[case("emptyIndexer.ts")]
#[case("emptyMemberAccess.ts")]
#[case("emptyModuleName.ts")]
#[case("emptyObjectNotSubtypeOfIndexSignatureContainingObject1.ts")]
#[case("emptyObjectNotSubtypeOfIndexSignatureContainingObject2.ts")]
#[case("emptyThenWarning.ts")]
#[case("emptyThenWithoutWarning.ts")]
#[case("emptyTypeArgumentList.ts")]
#[case("emptyTypeArgumentListWithNew.ts")]
#[case("ensureNoCrashExportAssignmentDefineProperrtyPotentialMerge.ts")] // NOT RUNNABLE
#[case("enumAssignmentCompat.ts")]
#[case("enumAssignmentCompat2.ts")]
#[case("enumAssignmentCompat3.ts")]
#[case("enumAssignmentCompat4.ts")]
#[case("enumAssignmentCompat5.ts")]
#[case("enumBasics1.ts")]
#[case("enumCodeGenNewLines1.ts")]
#[case("enumConflictsWithGlobalIdentifier.ts")]
#[case("enumDecl1.ts")]
#[case("enumDeclarationEmitInitializerHasImport.ts")] // NOT RUNNABLE
#[case("enumFromExternalModule.ts")] // NOT RUNNABLE
#[case("enumGenericTypeClash.ts")]
#[case("enumIdentifierLiterals.ts")]
#[case("enumIndexer.ts")]
#[case("enumInitializersWithExponents.ts")]
#[case("enumKeysQuotedAsObjectPropertiesInDeclarationEmit.ts")]
#[case("enumLiteralAssignableToEnumInsideUnion.ts")]
#[case("enumLiteralUnionNotWidened.ts")]
#[case("enumLiteralsSubtypeReduction.ts")]
#[case("enumMapBackIntoItself.ts")]
#[case("enumMemberResolution.ts")]
#[case("enumNegativeLiteral1.ts")]
#[case("enumNumbering1.ts")]
#[case("enumOperations.ts")]
#[case("enumPropertyAccess.ts")]
#[case("enumPropertyAccessBeforeInitalisation.ts")]
#[case("enumUsedBeforeDeclaration.ts")]
#[case("enumWithComputedMember.ts")]
#[case("enumWithInfinityProperty.ts")]
#[case("enumWithNaNProperty.ts")]
#[case("enumWithNegativeInfinityProperty.ts")]
#[case("enumWithParenthesizedInitializer1.ts")]
#[case("enumWithPrimitiveName.ts")]
#[case("enumWithQuotedElementName1.ts")]
#[case("enumWithQuotedElementName2.ts")]
#[case("enumWithUnicodeEscape1.ts")]
#[case("enumWithoutInitializerAfterComputedMember.ts")]
#[case("enumsWithMultipleDeclarations1.ts")]
#[case("enumsWithMultipleDeclarations2.ts")]
#[case("enumsWithMultipleDeclarations3.ts")]
#[case("errorConstructorSubtypes.ts")]
#[case("errorElaboration.ts")]
#[case("errorElaborationDivesIntoApparentlyPresentPropsOnly.ts")]
#[case("errorForBareSpecifierWithImplicitModuleResolutionNone.ts")]
#[case("errorForConflictingExportEqualsValue.ts")] // NOT RUNNABLE
#[case("errorForUsingPropertyOfTypeAsType01.ts")]
#[case("errorForUsingPropertyOfTypeAsType02.ts")]
#[case("errorForUsingPropertyOfTypeAsType03.ts")]
#[case("errorForwardReferenceForwadingConstructor.ts")]
#[case("errorHandlingInInstanceOf.ts")]
#[case("errorInfoForRelatedIndexTypesNoConstraintElaboration.ts")] // NOT RUNNABLE on .lib directive
#[case("errorLocationForInterfaceExtension.ts")]
#[case("errorMessageOnIntersectionsWithDiscriminants01.ts")]
#[case("errorMessageOnObjectLiteralType.ts")]
#[case("errorMessagesIntersectionTypes01.ts")]
#[case("errorMessagesIntersectionTypes02.ts")]
#[case("errorMessagesIntersectionTypes03.ts")]
#[case("errorMessagesIntersectionTypes04.ts")]
#[case("errorOnContextuallyTypedReturnType.ts")]
#[case("errorOnInitializerInInterfaceProperty.ts")]
#[case("errorOnInitializerInObjectTypeLiteralProperty.ts")]
#[case("errorOnUnionVsObjectShouldDeeplyDisambiguate.ts")]
#[case("errorOnUnionVsObjectShouldDeeplyDisambiguate2.ts")]
#[case("errorRecoveryInClassDeclaration.ts")]
#[case("errorRecoveryWithDotFollowedByNamespaceKeyword.ts")]
#[case("errorSpanForUnclosedJsxTag.tsx")]
#[case("errorSupression1.ts")]
#[case("errorTypesAsTypeArguments.ts")]
#[case("errorWithSameNameType.ts")] // NOT RUNNABLE
#[case("errorWithTruncatedType.ts")]
#[case("errorsForCallAndAssignmentAreSimilar.ts")] // FAILING 1872
#[case("errorsInGenericTypeReference.ts")]
#[case("errorsOnImportedSymbol.ts")] // NOT RUNNABLE
#[case("errorsOnUnionsOfOverlappingObjects01.ts")]
#[case("errorsWithInvokablesInUnions01.ts")]
#[case("es2015modulekind.ts")]
#[case("es2015modulekindWithES6Target.ts")]
#[case("es2017basicAsync.ts")]
#[case("es2018ObjectAssign.ts")]
#[case("es3-amd.ts")]
#[case("es3-declaration-amd.ts")]
#[case("es3-jsx-preserve.tsx")]
#[case("es3-jsx-react-native.tsx")]
#[case("es3-jsx-react.tsx")]
#[case("es3-oldStyleOctalLiteralInEnums.ts")]
#[case("es3-oldStyleOctalLiteralTypes.ts")]
#[case("es3-sourcemap-amd.ts")]
#[case("es3defaultAliasIsQuoted.ts")] // NOT RUNNABLE
#[case("es5-amd.ts")]
#[case("es5-asyncFunction.ts")]
#[case("es5-asyncFunctionArrayLiterals.ts")]
#[case("es5-asyncFunctionBinaryExpressions.ts")]
#[case("es5-asyncFunctionCallExpressions.ts")]
#[case("es5-asyncFunctionConditionals.ts")]
#[case("es5-asyncFunctionDoStatements.ts")]
#[case("es5-asyncFunctionElementAccess.ts")]
#[case("es5-asyncFunctionForInStatements.ts")]
#[case("es5-asyncFunctionForOfStatements.ts")]
#[case("es5-asyncFunctionForStatements.ts")]
#[case("es5-asyncFunctionHoisting.ts")]
#[case("es5-asyncFunctionIfStatements.ts")]
#[case("es5-asyncFunctionLongObjectLiteral.ts")]
#[case("es5-asyncFunctionNestedLoops.ts")]
#[case("es5-asyncFunctionNewExpressions.ts")]
#[case("es5-asyncFunctionObjectLiterals.ts")]
#[case("es5-asyncFunctionPropertyAccess.ts")]
#[case("es5-asyncFunctionReturnStatements.ts")]
#[case("es5-asyncFunctionSwitchStatements.ts")]
#[case("es5-asyncFunctionTryStatements.ts")]
#[case("es5-asyncFunctionWhileStatements.ts")]
#[case("es5-asyncFunctionWithStatements.ts")]
#[case("es5-commonjs.ts")]
#[case("es5-commonjs2.ts")]
#[case("es5-commonjs3.ts")]
#[case("es5-commonjs4.ts")]
#[case("es5-commonjs5.ts")]
#[case("es5-commonjs6.ts")]
#[case("es5-commonjs7.ts")] // NOT RUNNABLE
#[case("es5-commonjs8.ts")]
#[case("es5-declaration-amd.ts")]
#[case("es5-importHelpersAsyncFunctions.ts")] // NOT RUNNABLE
#[case("es5-oldStyleOctalLiteralInEnums.ts")]
#[case("es5-souremap-amd.ts")]
#[case("es5-system.ts")]
#[case("es5-system2.ts")]
#[case("es5-umd.ts")]
#[case("es5-umd2.ts")]
#[case("es5-umd3.ts")]
#[case("es5-umd4.ts")]
#[case("es5-yieldFunctionObjectLiterals.ts")]
#[case("es5ExportDefaultClassDeclaration.ts")]
#[case("es5ExportDefaultClassDeclaration2.ts")]
#[case("es5ExportDefaultClassDeclaration3.ts")]
#[case("es5ExportDefaultClassDeclaration4.ts")]
#[case("es5ExportDefaultExpression.ts")]
#[case("es5ExportDefaultFunctionDeclaration.ts")]
#[case("es5ExportDefaultFunctionDeclaration2.ts")]
#[case("es5ExportDefaultFunctionDeclaration3.ts")]
#[case("es5ExportDefaultFunctionDeclaration4.ts")]
#[case("es5ExportDefaultIdentifier.ts")]
#[case("es5ExportEquals.ts")]
#[case("es5ExportEqualsDts.ts")]
#[case("es5ModuleInternalNamedImports.ts")]
#[case("es5ModuleWithModuleGenAmd.ts")]
#[case("es5ModuleWithModuleGenCommonjs.ts")]
#[case("es5ModuleWithoutModuleGenTarget.ts")]
#[case("es5SetterparameterDestructuringNotElided.ts")]
#[case("es5andes6module.ts")]
#[case("es6-amd.ts")]
#[case("es6-declaration-amd.ts")]
#[case("es6-sourcemap-amd.ts")]
#[case("es6-umd.ts")]
#[case("es6-umd2.ts")]
#[case("es6ClassSuperCodegenBug.ts")]
#[case("es6ClassTest.ts")]
#[case("es6ClassTest2.ts")]
#[case("es6ClassTest3.ts")]
#[case("es6ClassTest4.ts")]
#[case("es6ClassTest5.ts")]
#[case("es6ClassTest7.ts")]
#[case("es6ClassTest8.ts")]
#[case("es6ClassTest9.ts")]
#[case("es6DeclOrdering.ts")]
#[case("es6ExportAll.ts")] // NOT RUNNABLE
#[case("es6ExportAllInEs5.ts")] // NOT RUNNABLE
#[case("es6ExportAssignment.ts")]
#[case("es6ExportAssignment2.ts")] // NOT RUNNABLE
#[case("es6ExportAssignment3.ts")] // NOT RUNNABLE
#[case("es6ExportAssignment4.ts")] // NOT RUNNABLE
#[case("es6ExportClause.ts")] // NOT RUNNABLE
#[case("es6ExportClauseInEs5.ts")] // NOT RUNNABLE
#[case("es6ExportClauseWithAssignmentInEs5.ts")] // NOT RUNNABLE
#[case("es6ExportClauseWithoutModuleSpecifier.ts")] // NOT RUNNABLE
#[case("es6ExportClauseWithoutModuleSpecifierInEs5.ts")] // NOT RUNNABLE
#[case("es6ExportDefaultClassDeclaration.ts")]
#[case("es6ExportDefaultClassDeclaration2.ts")]
#[case("es6ExportDefaultExpression.ts")]
#[case("es6ExportDefaultFunctionDeclaration.ts")]
#[case("es6ExportDefaultFunctionDeclaration2.ts")]
#[case("es6ExportDefaultIdentifier.ts")]
#[case("es6ExportEquals.ts")]
#[case("es6ExportEqualsInterop.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBinding.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingAmd.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingDts.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImport.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImport1.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImport1InEs5.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImport1WithExport.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImportDts.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImportDts1.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImportInEs5.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamedImportWithExport.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBinding.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBinding1.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBinding1InEs5.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBinding1WithExport.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBindingDts.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBindingDts1.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBindingInEs5.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingFollowedWithNamespaceBindingWithExport.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingInEs5.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingMergeErrors.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingNoDefaultProperty.ts")] // NOT RUNNABLE
#[case("es6ImportDefaultBindingWithExport.ts")] // NOT RUNNABLE
#[case("es6ImportEqualsDeclaration.ts")] // NOT RUNNABLE
#[case("es6ImportEqualsDeclaration2.ts")] // NOT RUNNABLE
#[case("es6ImportEqualsExportModuleCommonJsError.ts")] // NOT RUNNABLE
#[case("es6ImportEqualsExportModuleEs2015Error.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImport.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImportAmd.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImportDts.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImportInEs5.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImportMergeErrors.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImportNoNamedExports.ts")] // NOT RUNNABLE
#[case("es6ImportNameSpaceImportWithExport.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImport.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportAmd.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportDts.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportIdentifiersParsing.ts")]
#[case("es6ImportNamedImportInEs5.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportInExportAssignment.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportInIndirectExportAssignment.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportMergeErrors.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportNoExportMember.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportNoNamedExports.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportParsingError.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportWithExport.ts")] // NOT RUNNABLE
#[case("es6ImportNamedImportWithTypesAndValues.ts")] // NOT RUNNABLE
#[case("es6ImportParseErrors.ts")]
#[case("es6ImportWithoutFromClause.ts")] // NOT RUNNABLE
#[case("es6ImportWithoutFromClauseAmd.ts")] // NOT RUNNABLE
#[case("es6ImportWithoutFromClauseInEs5.ts")] // NOT RUNNABLE
#[case("es6ImportWithoutFromClauseNonInstantiatedModule.ts")] // NOT RUNNABLE
#[case("es6ImportWithoutFromClauseWithExport.ts")] // NOT RUNNABLE
#[case("es6MemberScoping.ts")]
#[case("es6Module.ts")]
#[case("es6ModuleClassDeclaration.ts")]
#[case("es6ModuleConst.ts")]
#[case("es6ModuleConstEnumDeclaration.ts")]
#[case("es6ModuleConstEnumDeclaration2.ts")]
#[case("es6ModuleEnumDeclaration.ts")]
#[case("es6ModuleFunctionDeclaration.ts")]
#[case("es6ModuleInternalImport.ts")]
#[case("es6ModuleInternalNamedImports.ts")]
#[case("es6ModuleInternalNamedImports2.ts")]
#[case("es6ModuleLet.ts")]
#[case("es6ModuleModuleDeclaration.ts")]
#[case("es6ModuleVariableStatement.ts")]
#[case("es6ModuleWithModuleGenTargetAmd.ts")]
#[case("es6ModuleWithModuleGenTargetCommonjs.ts")]
#[case("es6UseOfTopLevelRequire.ts")] // NOT RUNNABLE
#[case("esModuleInterop.ts")] // NOT RUNNABLE
#[case("esModuleInteropDefaultMemberMustBeSyntacticallyDefaultExport.ts")] // NOT RUNNABLE
#[case("esModuleInteropEnablesSyntheticDefaultImports.ts")] // NOT RUNNABLE
#[case("esModuleInteropImportCall.ts")] // NOT RUNNABLE
#[case("esModuleInteropImportDefaultWhenAllNamedAreDefaultAlias.ts")]
#[case("esModuleInteropImportNamespace.ts")] // NOT RUNNABLE
#[case("esModuleInteropImportTSLibHasImport.ts")] // NOT RUNNABLE
#[case("esModuleInteropNamedDefaultImports.ts")] // NOT RUNNABLE
#[case("esModuleInteropPrettyErrorRelatedInformation.ts")] // NOT RUNNABLE
#[case("esModuleInteropTslibHelpers.ts")] // NOT RUNNABLE
#[case("esModuleInteropUsesExportStarWhenDefaultPlusNames.ts")]
#[case("esModuleInteropWithExportStar.ts")] // NOT RUNNABLE
#[case("esModuleIntersectionCrash.ts")] // NOT RUNNABLE
#[case("esNextWeakRefs_IterableWeakMap.ts")]
#[case("escapedIdentifiers.ts")]
#[case("escapedReservedCompilerNamedIdentifier.ts")]
#[case("evalAfter0.ts")]
#[case("eventEmitterPatternWithRecordOfFunction.ts")]
#[case("evolvingArrayTypeInAssert.ts")]
#[case("exactSpellingSuggestion.ts")]
#[case("excessPropertyCheckWithEmptyObject.ts")]
#[case("excessPropertyCheckWithMultipleDiscriminants.ts")]
#[case("excessPropertyCheckWithNestedArrayIntersection.ts")]
#[case("excessPropertyCheckWithSpread.ts")]
#[case("excessPropertyCheckWithUnions.ts")]
#[case("excessPropertyChecksWithNestedIntersections.ts")]
#[case("excessPropertyErrorForFunctionTypes.ts")]
#[case("excessPropertyErrorsSuppressed.ts")]
#[case("excessiveStackDepthFlatArray.ts")] // NOT RUNNABLE
#[case("excessivelyLargeTupleSpread.ts")]
#[case("exhaustiveSwitchImplicitReturn.ts")]
#[case("exhaustiveSwitchWithWideningLiteralTypes.ts")]
#[case("expandoFunctionContextualTypes.ts")]
#[case("expandoFunctionContextualTypesJs.ts")] // NOT RUNNABLE
#[case("expandoFunctionContextualTypesNoValue.ts")]
#[case("experimentalDecoratorMetadataUnresolvedTypeObjectInEmit.ts")] // NOT RUNNABLE
#[case("explicitAnyAfterSpreadNoImplicitAnyError.ts")]
#[case("exportAlreadySeen.ts")]
#[case("exportArrayBindingPattern.ts")]
#[case("exportAsNamespace.d.ts")]
#[case("exportAsNamespaceConflict.ts")] // NOT RUNNABLE
#[case("exportAsNamespace_augment.ts")] // NOT RUNNABLE
#[case("exportAssignClassAndModule.ts")] // NOT RUNNABLE
#[case("exportAssignValueAndType.ts")]
#[case("exportAssignedNamespaceIsVisibleInDeclarationEmit.ts")] // NOT RUNNABLE
#[case("exportAssignedTypeAsTypeAnnotation.ts")] // NOT RUNNABLE
#[case("exportAssignmentClass.ts")] // NOT RUNNABLE
#[case("exportAssignmentEnum.ts")] // NOT RUNNABLE
#[case("exportAssignmentError.ts")] // NOT RUNNABLE
#[case("exportAssignmentFunction.ts")] // NOT RUNNABLE
#[case("exportAssignmentImportMergeNoCrash.ts")] // NOT RUNNABLE
#[case("exportAssignmentInterface.ts")] // NOT RUNNABLE
#[case("exportAssignmentInternalModule.ts")] // NOT RUNNABLE
#[case("exportAssignmentMembersVisibleInAugmentation.ts")] // NOT RUNNABLE
#[case("exportAssignmentOfDeclaredExternalModule.ts")] // NOT RUNNABLE
#[case("exportAssignmentOfGenericType1.ts")] // NOT RUNNABLE
#[case("exportAssignmentVariable.ts")] // NOT RUNNABLE
#[case("exportAssignmentWithDeclareAndExportModifiers.ts")]
#[case("exportAssignmentWithDeclareModifier.ts")]
#[case("exportAssignmentWithExportModifier.ts")]
#[case("exportAssignmentWithExports.ts")]
#[case("exportAssignmentWithImportStatementPrivacyError.ts")]
#[case("exportAssignmentWithPrivacyError.ts")]
#[case("exportAssignmentWithoutAllowSyntheticDefaultImportsError.ts")] // NOT RUNNABLE
#[case("exportAssignmentWithoutIdentifier1.ts")]
#[case("exportClassExtendingIntersection.ts")] // NOT RUNNABLE
#[case("exportClassWithoutName.ts")]
#[case("exportDeclarationInInternalModule.ts")]
#[case("exportDeclarationWithModuleSpecifierNameOnNextLine1.ts")] // NOT RUNNABLE
#[case("exportDeclarationsInAmbientNamespaces.ts")]
#[case("exportDeclarationsInAmbientNamespaces2.ts")]
#[case("exportDeclareClass1.ts")]
#[case("exportDefaultAbstractClass.ts")] // NOT RUNNABLE
#[case("exportDefaultAlias_excludesEverything.ts")]
#[case("exportDefaultAsyncFunction.ts")]
#[case("exportDefaultAsyncFunction2.ts")] // NOT RUNNABLE
#[case("exportDefaultClassInNamespace.ts")]
#[case("exportDefaultDuplicateCrash.ts")] // NOT RUNNABLE
#[case("exportDefaultForNonInstantiatedModule.ts")]
#[case("exportDefaultFunctionInNamespace.ts")]
#[case("exportDefaultImportedType.ts")] // NOT RUNNABLE
#[case("exportDefaultInterface.ts")] // NOT RUNNABLE
#[case("exportDefaultInterfaceAndTwoFunctions.ts")]
#[case("exportDefaultInterfaceAndValue.ts")]
#[case("exportDefaultMarksIdentifierAsUsed.ts")] // NOT RUNNABLE
#[case("exportDefaultMissingName.ts")]
#[case("exportDefaultParenthesize.ts")] // NOT RUNNABLE
#[case("exportDefaultParenthesizeES6.ts")] // NOT RUNNABLE
#[case("exportDefaultProperty.ts")] // NOT RUNNABLE
#[case("exportDefaultProperty2.ts")] // NOT RUNNABLE
#[case("exportDefaultQualifiedNameNoError.ts")] // NOT RUNNABLE
#[case("exportDefaultStripsFreshness.ts")] // NOT RUNNABLE
#[case("exportDefaultVariable.ts")]
#[case("exportDefaultWithJSDoc1.ts")] // NOT RUNNABLE
#[case("exportDefaultWithJSDoc2.ts")] // NOT RUNNABLE
#[case("exportEmptyArrayBindingPattern.ts")] // NOT RUNNABLE
#[case("exportEmptyObjectBindingPattern.ts")] // NOT RUNNABLE
#[case("exportEqualCallable.ts")] // NOT RUNNABLE
#[case("exportEqualErrorType.ts")] // NOT RUNNABLE
#[case("exportEqualMemberMissing.ts")] // NOT RUNNABLE
#[case("exportEqualNamespaces.ts")]
#[case("exportEqualsAmd.ts")]
#[case("exportEqualsClassNoRedeclarationError.ts")]
#[case("exportEqualsClassRedeclarationError.ts")]
#[case("exportEqualsCommonJs.ts")]
#[case("exportEqualsDefaultProperty.ts")] // NOT RUNNABLE
#[case("exportEqualsOfModule.ts")]
#[case("exportEqualsProperty.ts")] // NOT RUNNABLE
#[case("exportEqualsProperty2.ts")] // NOT RUNNABLE
#[case("exportEqualsUmd.ts")]
#[case("exportImport.ts")] // NOT RUNNABLE
#[case("exportImportAndClodule.ts")]
#[case("exportImportCanSubstituteConstEnumForValue.ts")]
#[case("exportImportMultipleFiles.ts")] // NOT RUNNABLE
#[case("exportImportNonInstantiatedModule.ts")]
#[case("exportImportNonInstantiatedModule2.ts")] // NOT RUNNABLE
#[case("exportInFunction.ts")]
#[case("exportNamespaceDeclarationRetainsVisibility.ts")]
#[case("exportObjectRest.ts")] // NOT RUNNABLE
#[case("exportPrivateType.ts")]
#[case("exportRedeclarationTypeAliases.ts")]
#[case("exportSameNameFuncVar.ts")]
#[case("exportSpecifierAndExportedMemberDeclaration.ts")]
#[case("exportSpecifierAndLocalMemberDeclaration.ts")]
#[case("exportSpecifierForAGlobal.ts")] // NOT RUNNABLE
#[case("exportSpecifierReferencingOuterDeclaration1.ts")]
#[case("exportSpecifierReferencingOuterDeclaration2.ts")] // NOT RUNNABLE
#[case("exportSpecifierReferencingOuterDeclaration3.ts")]
#[case("exportSpecifierReferencingOuterDeclaration4.ts")] // NOT RUNNABLE
#[case("exportStarForValues.ts")] // NOT RUNNABLE
#[case("exportStarForValues10.ts")] // NOT RUNNABLE
#[case("exportStarForValues2.ts")] // NOT RUNNABLE
#[case("exportStarForValues3.ts")] // NOT RUNNABLE
#[case("exportStarForValues4.ts")] // NOT RUNNABLE
#[case("exportStarForValues5.ts")] // NOT RUNNABLE
#[case("exportStarForValues6.ts")] // NOT RUNNABLE
#[case("exportStarForValues7.ts")] // NOT RUNNABLE
#[case("exportStarForValues8.ts")] // NOT RUNNABLE
#[case("exportStarForValues9.ts")] // NOT RUNNABLE
#[case("exportStarForValuesInSystem.ts")] // NOT RUNNABLE
#[case("exportStarFromEmptyModule.ts")] // NOT RUNNABLE
#[case("exportStarNotElided.ts")] // NOT RUNNABLE
#[case("exportToString.ts")]
#[case("exportVisibility.ts")]
#[case("exportedBlockScopedDeclarations.ts")]
#[case("exportedInterfaceInaccessibleInCallbackInModule.ts")]
#[case("exportedVariable1.ts")]
#[case("exportingContainingVisibleType.ts")]
#[case("exportsInAmbientModules1.ts")] // NOT RUNNABLE
#[case("exportsInAmbientModules2.ts")] // NOT RUNNABLE
#[case("expr.ts")]
#[case("expressionTypeNodeShouldError.ts")] // NOT RUNNABLE
#[case("expressionsForbiddenInParameterInitializers.ts")] // NOT RUNNABLE
#[case("extBaseClass1.ts")]
#[case("extBaseClass2.ts")]
#[case("extendAndImplementTheSameBaseType.ts")]
#[case("extendAndImplementTheSameBaseType2.ts")]
#[case("extendArray.ts")]
#[case("extendBaseClassBeforeItsDeclared.ts")]
#[case("extendConstructSignatureInInterface.ts")]
#[case("extendFromAny.ts")]
#[case("extendGenericArray.ts")]
#[case("extendGenericArray2.ts")]
#[case("extendGlobalThis.ts")] // NOT RUNNABLE
#[case("extendGlobalThis2.ts")]
#[case("extendNonClassSymbol1.ts")]
#[case("extendNonClassSymbol2.ts")]
#[case("extendPrivateConstructorClass.ts")]
#[case("extendedInterfaceGenericType.ts")]
#[case("extendedInterfacesWithDuplicateTypeParameters.ts")]
#[case("extendedUnicodeEscapeSequenceIdentifiers.ts")]
#[case("extendedUnicodePlaneIdentifiers.ts")]
#[case("extendedUnicodePlaneIdentifiersJSDoc.ts")] // NOT RUNNABLE
#[case("extendingClassFromAliasAndUsageInIndexer.ts")] // NOT RUNNABLE
#[case("extendingSetWithCheckJs.ts")]
#[case("extendsClauseAlreadySeen.ts")]
#[case("extendsClauseAlreadySeen2.ts")]
#[case("extendsJavaScript.ts")] // NOT RUNNABLE
#[case("extendsUntypedModule.ts")] // NOT RUNNABLE
#[case("extension.ts")]
#[case("externFunc.ts")]
#[case("externModule.ts")]
#[case("externModuleClobber.ts")]
#[case("externSemantics.ts")]
#[case("externSyntax.ts")]
#[case("externalModuleAssignToVar.ts")] // NOT RUNNABLE
#[case("externalModuleExportingGenericClass.ts")] // NOT RUNNABLE
#[case("externalModuleImmutableBindings.ts")] // NOT RUNNABLE
#[case("externalModuleQualification.ts")]
#[case("externalModuleReferenceDoubleUnderscore1.ts")]
#[case("externalModuleReferenceOfImportDeclarationWithExportModifier.ts")] // NOT RUNNABLE
#[case("externalModuleRefernceResolutionOrderInImportDeclaration.ts")] // NOT RUNNABLE
#[case("externalModuleResolution.ts")] // NOT RUNNABLE
#[case("externalModuleResolution2.ts")] // NOT RUNNABLE
#[case("externalModuleWithoutCompilerFlag1.ts")]
#[case("extractInferenceImprovement.ts")]
#[case("fallFromLastCase1.ts")]
#[case("fallFromLastCase2.ts")]
#[case("fallbackToBindingPatternForTypeInference.ts")]
#[case("fatArrowSelf.ts")]
#[case("fatArrowfunctionAsType.ts")]
#[case("fatarrowfunctions.ts")]
#[case("fatarrowfunctionsErrors.ts")]
#[case("fatarrowfunctionsInFunctionParameterDefaults.ts")]
#[case("fatarrowfunctionsInFunctions.ts")]
#[case("fatarrowfunctionsOptionalArgs.ts")]
#[case("fatarrowfunctionsOptionalArgsErrors1.ts")]
#[case("fatarrowfunctionsOptionalArgsErrors2.ts")]
#[case("fatarrowfunctionsOptionalArgsErrors3.ts")]
#[case("fatarrowfunctionsOptionalArgsErrors4.ts")]
#[case("fieldAndGetterWithSameName.ts")]
#[case("fileReferencesWithNoExtensions.ts")] // NOT RUNNABLE
#[case("fileWithNextLine1.ts")]
#[case("fileWithNextLine2.ts")]
#[case("fileWithNextLine3.ts")]
#[case("filesEmittingIntoSameOutput.ts")] // NOT RUNNABLE
#[case("filesEmittingIntoSameOutputWithOutOption.ts")] // NOT RUNNABLE
#[case("fillInMissingTypeArgsOnConstructCalls.ts")]
#[case("fillInMissingTypeArgsOnJSConstructCalls.ts")] // NOT RUNNABLE
#[case("fixTypeParameterInSignatureWithRestParameters.ts")]
#[case("fixingTypeParametersRepeatedly1.ts")]
#[case("fixingTypeParametersRepeatedly2.ts")]
#[case("fixingTypeParametersRepeatedly3.ts")]
#[case("flatArrayNoExcessiveStackDepth.ts")] // FAILING 2277
#[case("flowAfterFinally1.ts")]
#[case("flowControlTypeGuardThenSwitch.ts")]
#[case("flowInFinally1.ts")]
#[case("for.ts")]
#[case("forAwaitForUnion.ts")]
#[case("forIn.ts")]
#[case("forIn2.ts")]
#[case("forInModule.ts")]
#[case("forInStatement1.ts")]
#[case("forInStatement2.ts")]
#[case("forInStatement3.ts")]
#[case("forInStatement4.ts")]
#[case("forInStatement5.ts")]
#[case("forInStatement6.ts")]
#[case("forInStatement7.ts")]
#[case("forInStrictNullChecksNoError.ts")]
#[case("forLoopEndingMultilineComments.ts")]
#[case("forLoopWithDestructuringDoesNotElideFollowingStatement.ts")]
#[case("forOfStringConstituents.ts")]
#[case("forOfTransformsExpression.ts")]
#[case("forStatementInnerComments.ts")]
#[case("formatToPartsBigInt.ts")]
#[case("forwardDeclaredCommonTypes01.ts")]
#[case("forwardRefInClassProperties.ts")]
#[case("forwardRefInEnum.ts")]
#[case("forwardRefInTypeDeclaration.ts")]
#[case("freshLiteralTypesInIntersections.ts")]
#[case("fromAsIdentifier1.ts")]
#[case("fromAsIdentifier2.ts")]
#[case("funClodule.ts")]
#[case("funcdecl.ts")]
#[case("functionAndImportNameConflict.ts")] // NOT RUNNABLE
#[case("functionAndInterfaceWithSeparateErrors.ts")]
#[case("functionAndPropertyNameConflict.ts")]
#[case("functionArgShadowing.ts")]
#[case("functionAssignabilityWithArrayLike01.ts")] // NOT RUNNABLE
#[case("functionAssignment.ts")]
#[case("functionAssignmentError.ts")]
#[case("functionCall1.ts")]
#[case("functionCall10.ts")]
#[case("functionCall11.ts")]
#[case("functionCall12.ts")]
#[case("functionCall13.ts")]
#[case("functionCall14.ts")]
#[case("functionCall15.ts")]
#[case("functionCall16.ts")]
#[case("functionCall17.ts")]
#[case("functionCall18.ts")]
#[case("functionCall2.ts")]
#[case("functionCall3.ts")]
#[case("functionCall4.ts")]
#[case("functionCall5.ts")]
#[case("functionCall6.ts")]
#[case("functionCall7.ts")]
#[case("functionCall8.ts")]
#[case("functionCall9.ts")]
#[case("functionCallOnConstrainedTypeVariable.ts")]
#[case("functionDeclarationWithArgumentOfTypeFunctionTypeArray.ts")]
#[case("functionDeclarationWithResolutionOfTypeNamedArguments01.ts")]
#[case("functionDeclarationWithResolutionOfTypeOfSameName01.ts")]
#[case("functionExpressionAndLambdaMatchesFunction.ts")]
#[case("functionExpressionInWithBlock.ts")]
#[case("functionExpressionNames.ts")] // NOT RUNNABLE
#[case("functionExpressionReturningItself.ts")]
#[case("functionExpressionShadowedByParams.ts")]
#[case("functionExpressionWithResolutionOfTypeNamedArguments01.ts")]
#[case("functionExpressionWithResolutionOfTypeOfSameName01.ts")]
#[case("functionExpressionWithResolutionOfTypeOfSameName02.ts")]
#[case("functionInIfStatementInModule.ts")]
#[case("functionLikeInParameterInitializer.ts")]
#[case("functionMergedWithModule.ts")]
#[case("functionOnlyHasThrow.ts")]
#[case("functionOverloadAmbiguity1.ts")]
#[case("functionOverloadImplementationOfWrongName.ts")]
#[case("functionOverloadImplementationOfWrongName2.ts")]
#[case("functionOverloads.ts")]
#[case("functionOverloads1.ts")]
#[case("functionOverloads10.ts")]
#[case("functionOverloads11.ts")]
#[case("functionOverloads12.ts")]
#[case("functionOverloads13.ts")]
#[case("functionOverloads14.ts")]
#[case("functionOverloads15.ts")]
#[case("functionOverloads16.ts")]
#[case("functionOverloads17.ts")]
#[case("functionOverloads18.ts")]
#[case("functionOverloads19.ts")]
#[case("functionOverloads2.ts")]
#[case("functionOverloads20.ts")]
#[case("functionOverloads21.ts")]
#[case("functionOverloads22.ts")]
#[case("functionOverloads23.ts")]
#[case("functionOverloads24.ts")]
#[case("functionOverloads25.ts")]
#[case("functionOverloads26.ts")]
#[case("functionOverloads27.ts")]
#[case("functionOverloads28.ts")]
#[case("functionOverloads29.ts")]
#[case("functionOverloads3.ts")]
#[case("functionOverloads30.ts")]
#[case("functionOverloads31.ts")]
#[case("functionOverloads32.ts")]
#[case("functionOverloads33.ts")]
#[case("functionOverloads34.ts")]
#[case("functionOverloads35.ts")]
#[case("functionOverloads36.ts")]
#[case("functionOverloads37.ts")]
#[case("functionOverloads38.ts")]
#[case("functionOverloads39.ts")]
#[case("functionOverloads4.ts")]
#[case("functionOverloads40.ts")]
#[case("functionOverloads41.ts")]
#[case("functionOverloads42.ts")]
#[case("functionOverloads43.ts")]
#[case("functionOverloads44.ts")]
#[case("functionOverloads45.ts")]
#[case("functionOverloads5.ts")]
#[case("functionOverloads6.ts")]
#[case("functionOverloads7.ts")]
#[case("functionOverloads8.ts")]
#[case("functionOverloads9.ts")]
#[case("functionOverloadsOnGenericArity1.ts")]
#[case("functionOverloadsOnGenericArity2.ts")]
#[case("functionOverloadsOutOfOrder.ts")]
#[case("functionOverloadsRecursiveGenericReturnType.ts")]
#[case("functionParameterArityMismatch.ts")]
#[case("functionReturn.ts")]
#[case("functionReturnTypeQuery.ts")]
#[case("functionReturningItself.ts")]
#[case("functionSignatureAssignmentCompat1.ts")]
#[case("functionSubtypingOfVarArgs.ts")]
#[case("functionSubtypingOfVarArgs2.ts")]
#[case("functionType.ts")]
#[case("functionTypeArgumentArityErrors.ts")]
#[case("functionTypeArgumentArrayAssignment.ts")]
#[case("functionTypeArgumentAssignmentCompat.ts")]
#[case("functionTypesLackingReturnTypes.ts")]
#[case("functionVariableInReturnTypeAnnotation.ts")]
#[case("functionWithAnyReturnTypeAndNoReturnExpression.ts")]
#[case("functionWithDefaultParameterWithNoStatements1.ts")]
#[case("functionWithDefaultParameterWithNoStatements10.ts")]
#[case("functionWithDefaultParameterWithNoStatements11.ts")]
#[case("functionWithDefaultParameterWithNoStatements12.ts")]
#[case("functionWithDefaultParameterWithNoStatements13.ts")]
#[case("functionWithDefaultParameterWithNoStatements14.ts")]
#[case("functionWithDefaultParameterWithNoStatements15.ts")]
#[case("functionWithDefaultParameterWithNoStatements16.ts")]
#[case("functionWithDefaultParameterWithNoStatements2.ts")]
#[case("functionWithDefaultParameterWithNoStatements3.ts")]
#[case("functionWithDefaultParameterWithNoStatements4.ts")]
#[case("functionWithDefaultParameterWithNoStatements5.ts")]
#[case("functionWithDefaultParameterWithNoStatements6.ts")]
#[case("functionWithDefaultParameterWithNoStatements7.ts")]
#[case("functionWithDefaultParameterWithNoStatements8.ts")]
#[case("functionWithDefaultParameterWithNoStatements9.ts")]
#[case("functionWithNoBestCommonType1.ts")]
#[case("functionWithNoBestCommonType2.ts")]
#[case("functionWithSameNameAsField.ts")]
#[case("functionWithThrowButNoReturn1.ts")]
#[case("functionsInClassExpressions.ts")]
#[case("functionsMissingReturnStatementsAndExpressions.ts")]
#[case("functionsWithModifiersInBlocks1.ts")]
#[case("funduleExportedClassIsUsedBeforeDeclaration.ts")]
#[case("funduleOfFunctionWithoutReturnTypeAnnotation.ts")]
#[case("funduleSplitAcrossFiles.ts")] // NOT RUNNABLE
#[case("funduleUsedAcrossFileBoundary.ts")] // NOT RUNNABLE
#[case("fuzzy.ts")]
#[case("generativeRecursionWithTypeOf.ts")]
#[case("generatorES6InAMDModule.ts")]
#[case("generatorES6_1.ts")]
#[case("generatorES6_2.ts")]
#[case("generatorES6_3.ts")]
#[case("generatorES6_4.ts")]
#[case("generatorES6_5.ts")]
#[case("generatorES6_6.ts")]
#[case("generatorReturnExpressionIsChecked.ts")]
#[case("generatorTransformFinalLabel.ts")]
#[case("genericAndNonGenericInheritedSignature1.ts")]
#[case("genericAndNonGenericInheritedSignature2.ts")]
#[case("genericAndNonGenericOverload1.ts")]
#[case("genericArgumentCallSigAssignmentCompat.ts")]
#[case("genericArray0.ts")]
#[case("genericArray1.ts")]
#[case("genericArrayAssignment1.ts")]
#[case("genericArrayAssignmentCompatErrors.ts")]
#[case("genericArrayExtenstions.ts")]
#[case("genericArrayMethods1.ts")]
#[case("genericArrayPropertyAssignment.ts")]
#[case("genericArrayWithoutTypeAnnotation.ts")]
#[case("genericAssignmentCompatOfFunctionSignatures1.ts")]
#[case("genericAssignmentCompatWithInterfaces1.ts")]
#[case("genericBaseClassLiteralProperty.ts")]
#[case("genericBaseClassLiteralProperty2.ts")]
#[case("genericCallSpecializedToTypeArg.ts")]
#[case("genericCallWithFixedArguments.ts")]
#[case("genericCallWithNonGenericArgs1.ts")]
#[case("genericCallWithObjectLiteralArguments1.ts")]
#[case("genericCallWithoutArgs.ts")]
#[case("genericCallbackInvokedInsideItsContainingFunction1.ts")]
#[case("genericCallbacksAndClassHierarchy.ts")]
#[case("genericCallsWithoutParens.ts")]
#[case("genericCapturingFunctionNarrowing.ts")]
#[case("genericChainedCalls.ts")]
#[case("genericClassImplementingGenericInterfaceFromAnotherModule.ts")]
#[case("genericClassInheritsConstructorFromNonGenericClass.ts")]
#[case("genericClassPropertyInheritanceSpecialization.ts")]
#[case("genericClassStaticMethod.ts")]
#[case("genericClassWithStaticFactory.ts")]
#[case("genericClassWithStaticsUsingTypeArguments.ts")]
#[case("genericClasses0.ts")]
#[case("genericClasses1.ts")]
#[case("genericClasses2.ts")]
#[case("genericClasses3.ts")]
#[case("genericClasses4.ts")]
#[case("genericClassesInModule.ts")]
#[case("genericClassesInModule2.ts")]
#[case("genericClassesRedeclaration.ts")]
#[case("genericCloduleInModule.ts")]
#[case("genericCloduleInModule2.ts")]
#[case("genericCloneReturnTypes.ts")]
#[case("genericCloneReturnTypes2.ts")]
#[case("genericCombinators2.ts")]
#[case("genericConditionalConstrainedToUnknownNotAssignableToConcreteObject.ts")] // FAILING 2500
#[case("genericConstraint1.ts")]
#[case("genericConstraint2.ts")]
#[case("genericConstraint3.ts")]
#[case("genericConstraintDeclaration.ts")]
#[case("genericConstraintOnExtendedBuiltinTypes.ts")]
#[case("genericConstraintOnExtendedBuiltinTypes2.ts")]
#[case("genericConstraintSatisfaction1.ts")]
#[case("genericConstructExpressionWithoutArgs.ts")]
#[case("genericConstructInvocationWithNoTypeArg.ts")]
#[case("genericConstructSignatureInInterface.ts")]
#[case("genericConstructorFunction1.ts")]
#[case("genericContextualTypingSpecialization.ts")]
#[case("genericDefaults.ts")]
#[case("genericDefaultsErrors.ts")] // OUT OF SCOPE transformation error
#[case("genericDefaultsJs.ts")] // NOT RUNNABLE
#[case("genericDerivedTypeWithSpecializedBase.ts")]
#[case("genericDerivedTypeWithSpecializedBase2.ts")]
#[case("genericFunctionCallSignatureReturnTypeMismatch.ts")]
#[case("genericFunctionHasFreshTypeArgs.ts")]
#[case("genericFunctionInference1.ts")]
#[case("genericFunctionInference2.ts")]
#[case("genericFunctionSpecializations1.ts")]
#[case("genericFunctionTypedArgumentsAreFixed.ts")]
#[case("genericFunctions0.ts")]
#[case("genericFunctions1.ts")]
#[case("genericFunctions2.ts")]
#[case("genericFunctions3.ts")]
#[case("genericFunctionsAndConditionalInference.ts")]
#[case("genericFunctionsNotContextSensitive.ts")]
#[case("genericFunctionsWithOptionalParameters1.ts")]
#[case("genericFunctionsWithOptionalParameters2.ts")]
#[case("genericFunctionsWithOptionalParameters3.ts")]
#[case("genericFunduleInModule.ts")]
#[case("genericFunduleInModule2.ts")]
#[case("genericGetter.ts")]
#[case("genericGetter2.ts")]
#[case("genericGetter3.ts")]
#[case("genericImplements.ts")]
#[case("genericIndexTypeHasSensibleErrorMessage.ts")]
#[case("genericIndexedAccessMethodIntersectionCanBeAccessed.ts")]
#[case("genericInference1.ts")]
#[case("genericInference2.ts")]
#[case("genericInheritedDefaultConstructors.ts")]
#[case("genericInstanceOf.ts")]
#[case("genericInterfaceFunctionTypeParameter.ts")]
#[case("genericInterfaceImplementation.ts")]
#[case("genericInterfaceTypeCall.ts")]
#[case("genericInterfacesWithoutTypeArguments.ts")]
#[case("genericIsNeverEmptyObject.ts")]
#[case("genericLambaArgWithoutTypeArguments.ts")]
#[case("genericMemberFunction.ts")]
#[case("genericMergedDeclarationUsingTypeParameter.ts")]
#[case("genericMergedDeclarationUsingTypeParameter2.ts")]
#[case("genericMethodOverspecialization.ts")]
#[case("genericNewInterface.ts")]
#[case("genericNumberIndex.ts")]
#[case("genericObjectCreationWithoutTypeArgs.ts")]
#[case("genericObjectLitReturnType.ts")]
#[case("genericObjectSpreadResultInSwitch.ts")]
#[case("genericOfACloduleType1.ts")]
#[case("genericOfACloduleType2.ts")]
#[case("genericOverloadSignatures.ts")]
#[case("genericParameterAssignability1.ts")]
#[case("genericPrototypeProperty.ts")]
#[case("genericPrototypeProperty2.ts")]
#[case("genericPrototypeProperty3.ts")]
#[case("genericRecursiveImplicitConstructorErrors1.ts")]
#[case("genericRecursiveImplicitConstructorErrors2.ts")]
#[case("genericRecursiveImplicitConstructorErrors3.ts")]
#[case("genericReduce.ts")]
#[case("genericRestArgs.ts")]
#[case("genericRestTypes.ts")]
#[case("genericReturnTypeFromGetter1.ts")]
#[case("genericReversingTypeParameters.ts")]
#[case("genericReversingTypeParameters2.ts")]
#[case("genericSignatureIdentity.ts")]
#[case("genericSignatureInheritance.ts")]
#[case("genericSignatureInheritance2.ts")]
#[case("genericSpecializationToTypeLiteral1.ts")]
#[case("genericSpecializations1.ts")]
#[case("genericSpecializations2.ts")]
#[case("genericSpecializations3.ts")]
#[case("genericStaticAnyTypeFunction.ts")]
#[case("genericTemplateOverloadResolution.ts")]
#[case("genericTypeArgumentInference1.ts")]
#[case("genericTypeAssertions1.ts")]
#[case("genericTypeAssertions2.ts")]
#[case("genericTypeAssertions3.ts")]
#[case("genericTypeAssertions4.ts")]
#[case("genericTypeAssertions5.ts")]
#[case("genericTypeAssertions6.ts")]
#[case("genericTypeConstraints.ts")]
#[case("genericTypeParameterEquivalence2.ts")]
#[case("genericTypeReferencesRequireTypeArgs.ts")]
#[case("genericTypeUsedWithoutTypeArguments1.ts")]
#[case("genericTypeUsedWithoutTypeArguments3.ts")]
#[case("genericTypeWithCallableMembers.ts")]
#[case("genericTypeWithCallableMembers2.ts")]
#[case("genericTypeWithMultipleBases1.ts")]
#[case("genericTypeWithMultipleBases2.ts")]
#[case("genericTypeWithMultipleBases3.ts")]
#[case("genericTypeWithNonGenericBaseMisMatch.ts")]
#[case("genericWithCallSignatureReturningSpecialization.ts")]
#[case("genericWithCallSignatures1.ts")] // NOT RUNNABLE
#[case("genericWithIndexerOfTypeParameterType1.ts")]
#[case("genericWithIndexerOfTypeParameterType2.ts")]
#[case("genericWithOpenTypeParameters1.ts")]
#[case("generics0.ts")]
#[case("generics1.ts")]
#[case("generics1NoError.ts")]
#[case("generics2.ts")]
#[case("generics2NoError.ts")]
#[case("generics3.ts")]
#[case("generics4.ts")]
#[case("generics4NoError.ts")]
#[case("generics5.ts")]
#[case("genericsAndHigherOrderFunctions.ts")]
#[case("genericsManyTypeParameters.ts")]
#[case("genericsWithDuplicateTypeParameters1.ts")]
#[case("genericsWithoutTypeParameters1.ts")]
#[case("getAccessorWithImpliedReturnTypeAndFunctionClassMerge.ts")]
#[case("getAndSetAsMemberNames.ts")]
#[case("getAndSetNotIdenticalType.ts")]
#[case("getAndSetNotIdenticalType2.ts")]
#[case("getAndSetNotIdenticalType3.ts")]
#[case("getParameterNameAtPosition.ts")]
#[case("getSetEnumerable.ts")]
#[case("getsetReturnTypes.ts")]
#[case("getterControlFlowStrictNull.ts")]
#[case("getterErrorMessageNotDuplicated.ts")]
#[case("getterMissingReturnError.ts")]
#[case("getterSetterNonAccessor.ts")]
#[case("getterThatThrowsShouldNotNeedReturn.ts")]
#[case("gettersAndSetters.ts")]
#[case("gettersAndSettersAccessibility.ts")]
#[case("gettersAndSettersErrors.ts")]
#[case("gettersAndSettersTypesAgree.ts")]
#[case("giant.ts")]
#[case("global.ts")]
#[case("globalFunctionAugmentationOverload.ts")] // NOT RUNNABLE
#[case("globalIsContextualKeyword.ts")]
#[case("globalThis.ts")]
#[case("globalThisCapture.ts")]
#[case("globalThisDeclarationEmit.ts")] // NOT RUNNABLE
#[case("globalThisDeclarationEmit2.ts")] // NOT RUNNABLE
#[case("globalThisDeclarationEmit3.ts")] // NOT RUNNABLE
#[case("grammarAmbiguities1.ts")]
#[case("heterogeneousArrayAndOverloads.ts")]
#[case("hidingCallSignatures.ts")]
#[case("hidingConstructSignatures.ts")]
#[case("hidingIndexSignatures.ts")]
#[case("higherOrderMappedIndexLookupInference.ts")]
#[case("homomorphicMappedTypeIntersectionAssignability.ts")]
#[case("hugeDeclarationOutputGetsTruncatedWithError.ts")] // OUT OF SCOPE transformation error
#[case("i3.ts")]
#[case("icomparable.ts")]
#[case("idInProp.ts")]
#[case("identicalGenericConditionalsWithInferRelated.ts")]
#[case("identicalTypesNoDifferByCheckOrder.ts")]
#[case("identifierStartAfterNumericLiteral.ts")]
#[case("identityForSignaturesWithTypeParametersAndAny.ts")]
#[case("identityForSignaturesWithTypeParametersSwitched.ts")]
#[case("ifElseWithStatements1.ts")]
#[case("ifStatementInternalComments.ts")]
#[case("ignoredJsxAttributes.tsx")] // NOT RUNNABLE on .lib directive
#[case("illegalGenericWrapping1.ts")]
#[case("illegalModifiersOnClassElements.ts")]
#[case("illegalSuperCallsInConstructor.ts")]
#[case("implementArrayInterface.ts")]
#[case("implementClausePrecedingExtends.ts")]
#[case("implementGenericWithMismatchedTypes.ts")]
#[case("implementInterfaceAnyMemberWithVoid.ts")]
#[case("implementPublicPropertyAsPrivate.ts")]
#[case("implementsClauseAlreadySeen.ts")]
#[case("implementsInClassExpression.ts")]
#[case("implementsIncorrectlyNoAssertion.ts")]
#[case("implicitAnyAmbients.ts")]
#[case("implicitAnyAnyReturningFunction.ts")]
#[case("implicitAnyCastedValue.ts")]
#[case("implicitAnyDeclareFunctionExprWithoutFormalType.ts")]
#[case("implicitAnyDeclareFunctionWithoutFormalType.ts")]
#[case("implicitAnyDeclareFunctionWithoutFormalType2.ts")]
#[case("implicitAnyDeclareMemberWithoutType.ts")]
#[case("implicitAnyDeclareMemberWithoutType2.ts")]
#[case("implicitAnyDeclareTypePropertyWithoutType.ts")]
#[case("implicitAnyDeclareVariablesWithoutTypeAndInit.ts")]
#[case("implicitAnyFromCircularInference.ts")]
#[case("implicitAnyFunctionInvocationWithAnyArguements.ts")]
#[case("implicitAnyFunctionOverloadWithImplicitAnyReturnType.ts")]
#[case("implicitAnyFunctionReturnNullOrUndefined.ts")]
#[case("implicitAnyGenericTypeInference.ts")]
#[case("implicitAnyGenerics.ts")]
#[case("implicitAnyGetAndSetAccessorWithAnyReturnType.ts")]
#[case("implicitAnyInAmbientDeclaration.ts")]
#[case("implicitAnyInAmbientDeclaration2.d.ts")]
#[case("implicitAnyInCatch.ts")]
#[case("implicitAnyNewExprLackConstructorSignature.ts")]
#[case("implicitAnyWidenToAny.ts")]
#[case("implicitConstParameters.ts")]
#[case("implicitIndexSignatures.ts")]
#[case("importAliasAnExternalModuleInsideAnInternalModule.ts")] // NOT RUNNABLE
#[case("importAliasFromNamespace.ts")] // NOT RUNNABLE
#[case("importAliasWithDottedName.ts")]
#[case("importAnImport.ts")]
#[case("importAndVariableDeclarationConflict1.ts")]
#[case("importAndVariableDeclarationConflict2.ts")]
#[case("importAndVariableDeclarationConflict3.ts")]
#[case("importAndVariableDeclarationConflict4.ts")]
#[case("importAsBaseClass.ts")] // NOT RUNNABLE
#[case("importDecl.ts")] // NOT RUNNABLE
#[case("importDeclFromTypeNodeInJsSource.ts")] // NOT RUNNABLE
#[case("importDeclRefereingExternalModuleWithNoResolve.ts")]
#[case("importDeclTypes.ts")] // NOT RUNNABLE
#[case("importDeclWithClassModifiers.ts")]
#[case("importDeclWithDeclareModifier.ts")]
#[case("importDeclWithDeclareModifierInAmbientContext.ts")]
#[case("importDeclWithExportModifier.ts")]
#[case("importDeclWithExportModifierAndExportAssignment.ts")]
#[case("importDeclWithExportModifierAndExportAssignmentInAmbientContext.ts")]
#[case("importDeclWithExportModifierInAmbientContext.ts")]
#[case("importDeclarationInModuleDeclaration1.ts")]
#[case("importDeclarationNotCheckedAsValueWhenTargetNonValue.ts")] // NOT RUNNABLE
#[case("importDeclarationUsedAsTypeQuery.ts")] // NOT RUNNABLE
#[case("importEqualsError45874.ts")] // NOT RUNNABLE
#[case("importExportInternalComments.ts")] // NOT RUNNABLE
#[case("importHelpers.ts")] // NOT RUNNABLE
#[case("importHelpersAmd.ts")] // NOT RUNNABLE
#[case("importHelpersDeclarations.ts")] // NOT RUNNABLE
#[case("importHelpersES6.ts")] // NOT RUNNABLE
#[case("importHelpersInAmbientContext.ts")] // NOT RUNNABLE
#[case("importHelpersInIsolatedModules.ts")] // NOT RUNNABLE
#[case("importHelpersInTsx.tsx")] // NOT RUNNABLE
#[case("importHelpersNoHelpers.ts")] // NOT RUNNABLE
#[case("importHelpersNoHelpersForAsyncGenerators.ts")] // NOT RUNNABLE
#[case("importHelpersNoHelpersForPrivateFields.ts")] // NOT RUNNABLE
#[case("importHelpersNoModule.ts")] // NOT RUNNABLE
#[case("importHelpersOutFile.ts")] // NOT RUNNABLE
#[case("importHelpersSystem.ts")] // NOT RUNNABLE
#[case("importHelpersWithExportStarAs.ts")] // NOT RUNNABLE
#[case("importHelpersWithImportOrExportDefault.ts")] // NOT RUNNABLE
#[case("importHelpersWithImportOrExportDefaultNoTslib.1.ts")] // NOT RUNNABLE
#[case("importHelpersWithImportOrExportDefaultNoTslib.2.ts")] // NOT RUNNABLE
#[case("importHelpersWithImportOrExportDefaultNoTslib.3.ts")] // NOT RUNNABLE
#[case("importHelpersWithImportStarAs.ts")] // NOT RUNNABLE
#[case("importHelpersWithLocalCollisions.ts")] // NOT RUNNABLE
#[case("importInTypePosition.ts")]
#[case("importInsideModule.ts")] // NOT RUNNABLE
#[case("importNonExportedMember.ts")] // NOT RUNNABLE
#[case("importNonExportedMember1.ts")] // NOT RUNNABLE
#[case("importNonExportedMember10.ts")] // NOT RUNNABLE
#[case("importNonExportedMember11.ts")] // NOT RUNNABLE
#[case("importNonExportedMember2.ts")] // NOT RUNNABLE
#[case("importNonExportedMember3.ts")] // NOT RUNNABLE
#[case("importNonExportedMember4.ts")] // NOT RUNNABLE
#[case("importNonExportedMember5.ts")] // NOT RUNNABLE
#[case("importNonExportedMember6.ts")] // NOT RUNNABLE
#[case("importNonExportedMember7.ts")] // NOT RUNNABLE
#[case("importNonExportedMember8.ts")] // NOT RUNNABLE
#[case("importNonExportedMember9.ts")] // NOT RUNNABLE
#[case("importNotElidedWhenNotFound.ts")]
#[case("importOnAliasedIdentifiers.ts")]
#[case("importPropertyFromMappedType.ts")] // NOT RUNNABLE
#[case("importShadowsGlobalName.ts")] // NOT RUNNABLE
#[case("importShouldNotBeElidedInDeclarationEmit.ts")] // NOT RUNNABLE
#[case("importTypeGenericArrowTypeParenthesized.ts")] // NOT RUNNABLE
#[case("importTypeResolutionJSDocEOF.ts")] // NOT RUNNABLE
#[case("importTypeTypeofClassStaticLookup.ts")] // NOT RUNNABLE
#[case("importTypeWithUnparenthesizedGenericFunctionParsed.ts")]
#[case("importUsedAsTypeWithErrors.ts")] // NOT RUNNABLE
#[case("importUsedInExtendsList1.ts")] // NOT RUNNABLE
#[case("importUsedInGenericImportResolves.ts")] // NOT RUNNABLE
#[case("importWithTrailingSlash.ts")] // NOT RUNNABLE
#[case("importWithTrailingSlash_noResolve.ts")] // NOT RUNNABLE
#[case("import_reference-exported-alias.ts")] // NOT RUNNABLE
#[case("import_reference-to-type-alias.ts")] // NOT RUNNABLE
#[case("import_unneeded-require-when-referenecing-aliased-type-throug-array.ts")] // NOT RUNNABLE
#[case("import_var-referencing-an-imported-module-alias.ts")] // NOT RUNNABLE
#[case("importedAliasesInTypePositions.ts")] // NOT RUNNABLE
#[case("importedEnumMemberMergedWithExportedAliasIsError.ts")] // NOT RUNNABLE
#[case("importedModuleAddToGlobal.ts")]
#[case("importedModuleClassNameClash.ts")]
#[case("importsInAmbientModules1.ts")] // NOT RUNNABLE
#[case("importsInAmbientModules2.ts")] // NOT RUNNABLE
#[case("importsInAmbientModules3.ts")] // NOT RUNNABLE
#[case("inDoesNotOperateOnPrimitiveTypes.ts")]
#[case("inKeywordTypeguard.ts")]
#[case("inOperator.ts")]
#[case("inOperatorWithFunction.ts")]
#[case("inOperatorWithGeneric.ts")]
#[case("incompatibleAssignmentOfIdenticallyNamedTypes.ts")]
#[case("incompatibleExports1.ts")]
#[case("incompatibleExports2.ts")]
#[case("incompatibleGenericTypes.ts")]
#[case("incompatibleTypes.ts")]
#[case("incompleteDottedExpressionAtEOF.ts")]
#[case("incompleteObjectLiteral1.ts")]
#[case("incorrectClassOverloadChain.ts")]
#[case("incorrectNumberOfTypeArgumentsDuringErrorReporting.ts")]
#[case("incorrectRecursiveMappedTypeConstraint.ts")]
#[case("incrementOnNullAssertion.ts")]
#[case("incrementOnTypeParameter.ts")]
#[case("incrementalConfig.ts")] // NOT RUNNABLE
#[case("incrementalInvalid.ts")]
#[case("incrementalOut.ts")]
#[case("incrementalTsBuildInfoFile.ts")] // NOT RUNNABLE
#[case("indexClassByNumber.ts")]
#[case("indexIntoArraySubclass.ts")]
#[case("indexIntoEnum.ts")]
#[case("indexSignatureAndMappedType.ts")]
#[case("indexSignatureMustHaveTypeAnnotation.ts")]
#[case("indexSignatureOfTypeUnknownStillRequiresIndexSignature.ts")]
#[case("indexSignatureTypeCheck.ts")]
#[case("indexSignatureTypeCheck2.ts")]
#[case("indexSignatureWithAccessibilityModifier.ts")]
#[case("indexSignatureWithInitializer.ts")]
#[case("indexSignatureWithInitializer1.ts")]
#[case("indexSignatureWithTrailingComma.ts")]
#[case("indexSignatureWithoutTypeAnnotation1..ts")]
#[case("indexSignatureWithoutTypeAnnotation1.ts")]
#[case("indexSignaturesInferentialTyping.ts")]
#[case("indexTypeCheck.ts")]
#[case("indexWithUndefinedAndNull.ts")]
#[case("indexWithUndefinedAndNullStrictNullChecks.ts")]
#[case("indexWithoutParamType.ts")]
#[case("indexWithoutParamType2.ts")]
#[case("indexedAccessCanBeHighOrder.ts")]
#[case("indexedAccessImplicitlyAny.ts")]
#[case("indexedAccessKeyofNestedSimplifiedSubstituteUnwrapped.ts")]
#[case("indexedAccessNormalization.ts")]
#[case("indexedAccessPrivateMemberOfGenericConstraint.ts")]
#[case("indexedAccessRelation.ts")]
#[case("indexedAccessRetainsIndexSignature.ts")]
#[case("indexedAccessToThisTypeOnIntersection01.ts")]
#[case("indexedAccessTypeConstraints.ts")]
#[case("indexedAccessWithFreshObjectLiteral.ts")]
#[case("indexer.ts")]
#[case("indexer2.ts")]
#[case("indexer2A.ts")]
#[case("indexer3.ts")]
#[case("indexerA.ts")]
#[case("indexerAsOptional.ts")]
#[case("indexerAssignability.ts")]
#[case("indexerConstraints.ts")]
#[case("indexerConstraints2.ts")]
#[case("indexerReturningTypeParameter1.ts")]
#[case("indexerSignatureWithRestParam.ts")]
#[case("indexingTypesWithNever.ts")]
#[case("indirectGlobalSymbolPartOfObjectType.ts")]
#[case("indirectSelfReference.ts")]
#[case("indirectSelfReferenceGeneric.ts")]
#[case("indirectTypeParameterReferences.ts")]
#[case("indirectUniqueSymbolDeclarationEmit.ts")]
#[case("inferConditionalConstraintMappedMember.ts")] // FAILING 2853
#[case("inferFromGenericFunctionReturnTypes1.ts")]
#[case("inferFromGenericFunctionReturnTypes2.ts")]
#[case("inferFromGenericFunctionReturnTypes3.ts")]
#[case("inferObjectTypeFromStringLiteralToKeyof.ts")]
#[case("inferParameterWithMethodCallInitializer.ts")]
#[case("inferSecondaryParameter.ts")]
#[case("inferSetterParamType.ts")]
#[case("inferTInParentheses.ts")]
#[case("inferTypeArgumentsInSignatureWithRestParameters.ts")]
#[case("inferTypeParameterConstraints.ts")]
#[case("inferenceAndSelfReferentialConstraint.ts")]
#[case("inferenceDoesntCompareAgainstUninstantiatedTypeParameter.ts")]
#[case("inferenceErasedSignatures.ts")]
#[case("inferenceFromIncompleteSource.ts")]
#[case("inferenceFromParameterlessLambda.ts")]
#[case("inferenceLimit.ts")] // NOT RUNNABLE
#[case("inferenceOptionalProperties.ts")]
#[case("inferenceOptionalPropertiesStrict.ts")]
#[case("inferenceOptionalPropertiesToIndexSignatures.ts")]
#[case("inferenceShouldFailOnEvolvingArrays.ts")]
#[case("inferenceUnionOfObjectsMappedContextualType.ts")]
#[case("inferentialTypingObjectLiteralMethod1.ts")]
#[case("inferentialTypingObjectLiteralMethod2.ts")]
#[case("inferentialTypingUsingApparentType1.ts")]
#[case("inferentialTypingUsingApparentType2.ts")]
#[case("inferentialTypingUsingApparentType3.ts")]
#[case("inferentialTypingWithFunctionType.ts")]
#[case("inferentialTypingWithFunctionType2.ts")]
#[case("inferentialTypingWithFunctionTypeNested.ts")]
#[case("inferentialTypingWithFunctionTypeSyntacticScenarios.ts")]
#[case("inferentialTypingWithFunctionTypeZip.ts")]
#[case("inferentialTypingWithObjectLiteralProperties.ts")]
#[case("inferentiallyTypingAnEmptyArray.ts")]
#[case("inferredFunctionReturnTypeIsEmptyType.ts")]
#[case("inferredIndexerOnNamespaceImport.ts")] // NOT RUNNABLE
#[case("inferredNonidentifierTypesGetQuotes.ts")]
#[case("inferrenceInfiniteLoopWithSubtyping.ts")] // NOT RUNNABLE
#[case("inferringAnyFunctionType1.ts")]
#[case("inferringAnyFunctionType2.ts")]
#[case("inferringAnyFunctionType3.ts")]
#[case("inferringAnyFunctionType4.ts")]
#[case("inferringAnyFunctionType5.ts")]
#[case("infiniteConstraints.ts")]
#[case("infiniteExpandingTypeThroughInheritanceInstantiation.ts")]
#[case("infinitelyExpandingBaseTypes1.ts")]
#[case("infinitelyExpandingBaseTypes2.ts")]
#[case("infinitelyExpandingOverloads.ts")]
#[case("infinitelyExpandingTypeAssignability.ts")]
#[case("infinitelyExpandingTypes1.ts")]
#[case("infinitelyExpandingTypes2.ts")]
#[case("infinitelyExpandingTypes3.ts")]
#[case("infinitelyExpandingTypes4.ts")]
#[case("infinitelyExpandingTypes5.ts")]
#[case("infinitelyExpandingTypesNonGenericBase.ts")]
#[case("infinitelyGenerativeInheritance1.ts")]
#[case("inheritFromGenericTypeParameter.ts")]
#[case("inheritSameNamePrivatePropertiesFromDifferentOrigins.ts")]
#[case("inheritSameNamePrivatePropertiesFromSameOrigin.ts")]
#[case("inheritSameNamePropertiesWithDifferentOptionality.ts")]
#[case("inheritSameNamePropertiesWithDifferentVisibility.ts")]
#[case("inheritance.ts")]
#[case("inheritance1.ts")]
#[case("inheritanceGrandParentPrivateMemberCollision.ts")]
#[case("inheritanceGrandParentPrivateMemberCollisionWithPublicMember.ts")]
#[case("inheritanceGrandParentPublicMemberCollisionWithPrivateMember.ts")]
#[case("inheritanceMemberAccessorOverridingAccessor.ts")]
#[case("inheritanceMemberAccessorOverridingMethod.ts")]
#[case("inheritanceMemberAccessorOverridingProperty.ts")]
#[case("inheritanceMemberFuncOverridingAccessor.ts")]
#[case("inheritanceMemberFuncOverridingMethod.ts")]
#[case("inheritanceMemberFuncOverridingProperty.ts")]
#[case("inheritanceMemberPropertyOverridingAccessor.ts")]
#[case("inheritanceMemberPropertyOverridingMethod.ts")]
#[case("inheritanceMemberPropertyOverridingProperty.ts")]
#[case("inheritanceOfGenericConstructorMethod1.ts")]
#[case("inheritanceOfGenericConstructorMethod2.ts")]
#[case("inheritanceStaticAccessorOverridingAccessor.ts")]
#[case("inheritanceStaticAccessorOverridingMethod.ts")]
#[case("inheritanceStaticAccessorOverridingProperty.ts")]
#[case("inheritanceStaticFuncOverridingAccessor.ts")]
#[case("inheritanceStaticFuncOverridingAccessorOfFuncType.ts")]
#[case("inheritanceStaticFuncOverridingMethod.ts")]
#[case("inheritanceStaticFuncOverridingProperty.ts")]
#[case("inheritanceStaticFuncOverridingPropertyOfFuncType.ts")]
#[case("inheritanceStaticFunctionOverridingInstanceProperty.ts")]
#[case("inheritanceStaticMembersCompatible.ts")]
#[case("inheritanceStaticMembersIncompatible.ts")]
#[case("inheritanceStaticPropertyOverridingAccessor.ts")]
#[case("inheritanceStaticPropertyOverridingMethod.ts")]
#[case("inheritanceStaticPropertyOverridingProperty.ts")]
#[case("inheritedConstructorPropertyContextualType.ts")]
#[case("inheritedConstructorWithRestParams.ts")]
#[case("inheritedConstructorWithRestParams2.ts")]
#[case("inheritedFunctionAssignmentCompatibility.ts")]
#[case("inheritedGenericCallSignature.ts")]
#[case("inheritedMembersAndIndexSignaturesFromDifferentBases.ts")]
#[case("inheritedMembersAndIndexSignaturesFromDifferentBases2.ts")]
#[case("inheritedModuleMembersForClodule.ts")]
#[case("inheritedOverloadedSpecializedSignatures.ts")]
#[case("inheritedStringIndexersFromDifferentBaseTypes.ts")]
#[case("inheritedStringIndexersFromDifferentBaseTypes2.ts")]
#[case("initializePropertiesWithRenamedLet.ts")]
#[case("initializedDestructuringAssignmentTypes.ts")]
#[case("initializedParameterBeforeNonoptionalNotOptional.ts")] // NOT RUNNABLE
#[case("initializerWithThisPropertyAccess.ts")]
#[case("initializersInAmbientEnums.ts")]
#[case("inlineConditionalHasSimilarAssignability.ts")]
#[case("inlineSourceMap.ts")]
#[case("inlineSourceMap2.ts")]
#[case("inlineSources.ts")] // NOT RUNNABLE
#[case("inlineSources2.ts")] // NOT RUNNABLE
#[case("inlinedAliasAssignableToConstraintSameAsAlias.ts")]
#[case("innerAliases.ts")]
#[case("innerAliases2.ts")]
#[case("innerBoundLambdaEmit.ts")]
#[case("innerExtern.ts")]
#[case("innerFunc.ts")]
#[case("innerModExport1.ts")]
#[case("innerModExport2.ts")]
#[case("innerOverloads.ts")]
#[case("innerTypeArgumentInference.ts")]
#[case("innerTypeCheckOfLambdaArgument.ts")]
#[case("instanceAndStaticDeclarations1.ts")]
#[case("instanceOfAssignability.ts")]
#[case("instanceOfInExternalModules.ts")] // NOT RUNNABLE
#[case("instanceSubtypeCheck1.ts")]
#[case("instanceSubtypeCheck2.ts")]
#[case("instanceofNarrowReadonlyArray.ts")]
#[case("instanceofOperator.ts")]
#[case("instanceofWithPrimitiveUnion.ts")]
#[case("instanceofWithStructurallyIdenticalTypes.ts")]
#[case("instantiateConstraintsToTypeArguments2.ts")]
#[case("instantiateContextualTypes.ts")]
#[case("instantiateContextuallyTypedGenericThis.ts")]
#[case("instantiateCrossFileMerge.ts")] // NOT RUNNABLE
#[case("instantiateTypeParameter.ts")]
#[case("instantiatedBaseTypeConstraints.ts")]
#[case("instantiatedBaseTypeConstraints2.ts")]
#[case("instantiatedReturnTypeContravariance.ts")]
#[case("instantiatedTypeAliasDisplay.ts")]
#[case("intTypeCheck.ts")]
#[case("interMixingModulesInterfaces0.ts")]
#[case("interMixingModulesInterfaces1.ts")]
#[case("interMixingModulesInterfaces2.ts")]
#[case("interMixingModulesInterfaces3.ts")]
#[case("interMixingModulesInterfaces4.ts")]
#[case("interMixingModulesInterfaces5.ts")]
#[case("interface0.ts")]
#[case("interfaceAssignmentCompat.ts")]
#[case("interfaceClassMerging.ts")]
#[case("interfaceClassMerging2.ts")]
#[case("interfaceContextualType.ts")]
#[case("interfaceDeclaration1.ts")]
#[case("interfaceDeclaration2.ts")]
#[case("interfaceDeclaration3.ts")]
#[case("interfaceDeclaration4.ts")]
#[case("interfaceDeclaration5.ts")]
#[case("interfaceDeclaration6.ts")]
#[case("interfaceExtendsClass1.ts")]
#[case("interfaceExtendsClassWithPrivate1.ts")]
#[case("interfaceExtendsClassWithPrivate2.ts")]
#[case("interfaceImplementation1.ts")]
#[case("interfaceImplementation2.ts")]
#[case("interfaceImplementation3.ts")]
#[case("interfaceImplementation4.ts")]
#[case("interfaceImplementation5.ts")]
#[case("interfaceImplementation6.ts")]
#[case("interfaceImplementation7.ts")]
#[case("interfaceImplementation8.ts")]
#[case("interfaceInReopenedModule.ts")]
#[case("interfaceInheritance.ts")]
#[case("interfaceInheritance2.ts")]
#[case("interfaceMayNotBeExtendedWitACall.ts")]
#[case("interfaceMemberValidation.ts")]
#[case("interfaceMergedUnconstrainedNoErrorIrrespectiveOfOrder.ts")] // NOT RUNNABLE
#[case("interfaceNameAsIdentifier.ts")]
#[case("interfaceNaming1.ts")]
#[case("interfaceOnly.ts")]
#[case("interfacePropertiesWithSameName1.ts")]
#[case("interfacePropertiesWithSameName2.ts")]
#[case("interfacePropertiesWithSameName3.ts")]
#[case("interfaceSubtyping.ts")]
#[case("interfaceWithCommaSeparators.ts")]
#[case("interfaceWithImplements1.ts")]
#[case("interfaceWithMultipleDeclarations.ts")]
#[case("interfaceWithOptionalProperty.ts")]
#[case("interfacedecl.ts")]
#[case("interfacedeclWithIndexerErrors.ts")]
#[case("internalAliasClass.ts")]
#[case("internalAliasClassInsideLocalModuleWithExport.ts")]
#[case("internalAliasClassInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasClassInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasClassInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasClassInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasEnum.ts")]
#[case("internalAliasEnumInsideLocalModuleWithExport.ts")]
#[case("internalAliasEnumInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasEnumInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasEnumInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasEnumInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasFunction.ts")]
#[case("internalAliasFunctionInsideLocalModuleWithExport.ts")]
#[case("internalAliasFunctionInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasFunctionInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasFunctionInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasFunctionInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasInitializedModule.ts")]
#[case("internalAliasInitializedModuleInsideLocalModuleWithExport.ts")]
#[case("internalAliasInitializedModuleInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasInitializedModuleInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasInitializedModuleInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasInitializedModuleInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasInterface.ts")]
#[case("internalAliasInterfaceInsideLocalModuleWithExport.ts")]
#[case("internalAliasInterfaceInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasInterfaceInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasInterfaceInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasInterfaceInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasUninitializedModule.ts")]
#[case("internalAliasUninitializedModuleInsideLocalModuleWithExport.ts")]
#[case("internalAliasUninitializedModuleInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasUninitializedModuleInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasUninitializedModuleInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasUninitializedModuleInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasVar.ts")]
#[case("internalAliasVarInsideLocalModuleWithExport.ts")]
#[case("internalAliasVarInsideLocalModuleWithoutExport.ts")]
#[case("internalAliasVarInsideLocalModuleWithoutExportAccessError.ts")]
#[case("internalAliasVarInsideTopLevelModuleWithExport.ts")]
#[case("internalAliasVarInsideTopLevelModuleWithoutExport.ts")]
#[case("internalAliasWithDottedNameEmit.ts")]
#[case("internalImportInstantiatedModuleMergedWithClassNotReferencingInstance.ts")]
#[case("internalImportInstantiatedModuleMergedWithClassNotReferencingInstanceNoConflict.ts")]
#[case("internalImportInstantiatedModuleNotReferencingInstance.ts")]
#[case("internalImportUnInstantiatedModuleMergedWithClassNotReferencingInstance.ts")]
#[case("internalImportUnInstantiatedModuleMergedWithClassNotReferencingInstanceNoConflict.ts")]
#[case("internalImportUnInstantiatedModuleNotReferencingInstanceNoConflict.ts")]
#[case("intersectionOfMixinConstructorTypeAndNonConstructorType.ts")]
#[case("intersectionOfTypeVariableHasApparentSignatures.ts")]
#[case("intersectionPropertyCheck.ts")]
#[case("intersectionTypeInference1.ts")]
#[case("intersectionTypeNormalization.ts")]
#[case("intersectionTypeWithLeadingOperator.ts")]
#[case("intersectionType_useDefineForClassFields.ts")]
#[case("intersectionWithConflictingPrivates.ts")]
#[case("intersectionsAndOptionalProperties.ts")]
#[case("intersectionsAndReadonlyProperties.ts")]
#[case("intersectionsOfLargeUnions.ts")]
#[case("intersectionsOfLargeUnions2.ts")]
#[case("intrinsics.ts")] // NOT IN SCOPE transformer error
#[case("invalidConstraint1.ts")]
#[case("invalidContinueInDownlevelAsync.ts")]
#[case("invalidLetInForOfAndForIn_ES5.ts")]
#[case("invalidLetInForOfAndForIn_ES6.ts")]
#[case("invalidReferenceSyntax1.ts")]
#[case("invalidSplice.ts")]
#[case("invalidStaticField.ts")]
#[case("invalidSymbolInTypeParameter1.ts")]
#[case("invalidThisEmitInContextualObjectLiteral.ts")]
#[case("invalidTripleSlashReference.ts")]
#[case("invalidTypeNames.ts")]
#[case("invalidUnicodeEscapeSequance.ts")]
#[case("invalidUnicodeEscapeSequance2.ts")]
#[case("invalidUnicodeEscapeSequance3.ts")]
#[case("invalidUnicodeEscapeSequance4.ts")]
#[case("invalidUseOfTypeAsNamespace.ts")]
#[case("invariantGenericErrorElaboration.ts")]
#[case("invocationExpressionInFunctionParameter.ts")]
#[case("invokingNonGenericMethodWithTypeArguments1.ts")]
#[case("invokingNonGenericMethodWithTypeArguments2.ts")]
#[case("ipromise2.ts")]
#[case("ipromise3.ts")]
#[case("ipromise4.ts")]
#[case("isArray.ts")]
#[case("isDeclarationVisibleNodeKinds.ts")]
#[case("isLiteral1.ts")]
#[case("isLiteral2.ts")]
#[case("isolatedModulesAmbientConstEnum.ts")] // NOT RUNNABLE
#[case("isolatedModulesDeclaration.ts")] // NOT RUNNABLE
#[case("isolatedModulesDontElideReExportStar.ts")] // NOT RUNNABLE
#[case("isolatedModulesES6.ts")] // NOT RUNNABLE
#[case("isolatedModulesImportConstEnum.ts")] // NOT RUNNABLE
#[case("isolatedModulesImportConstEnumTypeOnly.ts")] // NOT RUNNABLE
#[case("isolatedModulesImportExportElision.ts")] // NOT RUNNABLE
#[case("isolatedModulesNoEmitOnError.ts")] // NOT RUNNABLE
#[case("isolatedModulesNoExternalModule.ts")] // NOT RUNNABLE
#[case("isolatedModulesNonAmbientConstEnum.ts")] // NOT RUNNABLE
#[case("isolatedModulesOut.ts")] // NOT RUNNABLE
#[case("isolatedModulesPlainFile-AMD.ts")]
#[case("isolatedModulesPlainFile-CommonJS.ts")]
#[case("isolatedModulesPlainFile-ES6.ts")]
#[case("isolatedModulesPlainFile-System.ts")]
#[case("isolatedModulesPlainFile-UMD.ts")]
#[case("isolatedModulesReExportType.ts")] // NOT RUNNABLE
#[case("isolatedModulesRequiresPreserveConstEnum.ts")] // NOT RUNNABLE
#[case("isolatedModulesSourceMap.ts")] // NOT RUNNABLE
#[case("isolatedModulesSpecifiedModule.ts")] // NOT RUNNABLE
#[case("isolatedModulesUnspecifiedModule.ts")] // NOT RUNNABLE
#[case("isolatedModulesWithDeclarationFile.ts")] // NOT RUNNABLE
#[case("isolatedModules_resolveJsonModule.ts")] // NOT RUNNABLE
#[case("isolatedModules_resolveJsonModule_strict_outDir_commonJs.ts")] // NOT RUNNABLE
#[case("iteratorsAndStrictNullChecks.ts")]
#[case("javascriptCommonjsModule.ts")] // NOT RUNNABLE
#[case("javascriptDefinePropertyPrototypeNonConstructor.ts")] // NOT RUNNABLE
#[case("javascriptImportDefaultBadExport.ts")] // NOT RUNNABLE
#[case("javascriptThisAssignmentInStaticBlock.ts")] // NOT RUNNABLE
#[case("jqueryInference.ts")]
#[case("jsCheckObjectDefineThisNoCrash.ts")] // NOT RUNNABLE
#[case("jsDeclarationsWithDefaultAsNamespaceLikeMerge.ts")] // NOT RUNNABLE
#[case("jsElementAccessNoContextualTypeCrash.ts")] // NOT RUNNABLE
#[case("jsEmitIntersectionProperty.ts")] // NOT RUNNABLE
#[case("jsEnumCrossFileExport.ts")] // NOT RUNNABLE
#[case("jsEnumFunctionLocalNoCrash.ts")] // NOT RUNNABLE
#[case("jsEnumTagOnObjectFrozen.ts")] // NOT RUNNABLE
#[case("jsExpandoObjectDefineProperty.ts")] // NOT RUNNABLE
#[case("jsExportMemberMergedWithModuleAugmentation.ts")] // NOT RUNNABLE
#[case("jsExportMemberMergedWithModuleAugmentation2.ts")] // NOT RUNNABLE
#[case("jsExportMemberMergedWithModuleAugmentation3.ts")] // NOT RUNNABLE
#[case("jsExtendsImplicitAny.ts")] // NOT RUNNABLE
#[case("jsFileClassPropertyInitalizationInObjectLiteral.ts")] // NOT RUNNABLE
#[case("jsFileClassPropertyType.ts")] // NOT RUNNABLE
#[case("jsFileClassPropertyType2.ts")] // NOT RUNNABLE
#[case("jsFileClassPropertyType3.ts")] // NOT RUNNABLE
#[case("jsFileClassSelfReferencedProperty.ts")] // NOT RUNNABLE
#[case("jsFileCompilationAbstractModifier.ts")] // NOT RUNNABLE
#[case("jsFileCompilationAmbientVarDeclarationSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationAwaitModifier.ts")] // NOT RUNNABLE
#[case("jsFileCompilationBindDeepExportsAssignment.ts")] // NOT RUNNABLE
#[case("jsFileCompilationBindDuplicateIdentifier.ts")] // NOT RUNNABLE
#[case("jsFileCompilationBindErrors.ts")] // NOT RUNNABLE
#[case("jsFileCompilationBindMultipleDefaultExports.ts")] // NOT RUNNABLE
#[case("jsFileCompilationBindReachabilityErrors.ts")] // NOT RUNNABLE
#[case("jsFileCompilationBindStrictModeErrors.ts")] // NOT RUNNABLE
#[case("jsFileCompilationClassMethodContainingArrowFunction.ts")] // NOT RUNNABLE
#[case("jsFileCompilationConstModifier.ts")] // NOT RUNNABLE
#[case("jsFileCompilationDecoratorSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationDuplicateFunctionImplementation.ts")] // NOT RUNNABLE
#[case("jsFileCompilationDuplicateFunctionImplementationFileOrderReversed.ts")] // NOT RUNNABLE
#[case("jsFileCompilationDuplicateVariable.ts")] // NOT RUNNABLE
#[case("jsFileCompilationDuplicateVariableErrorReported.ts")] // NOT RUNNABLE
#[case("jsFileCompilationEmitBlockedCorrectly.ts")] // NOT RUNNABLE
#[case("jsFileCompilationEmitDeclarations.ts")] // NOT RUNNABLE
#[case("jsFileCompilationEmitTrippleSlashReference.ts")] // NOT RUNNABLE
#[case("jsFileCompilationEnumSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationErrorOnDeclarationsWithJsFileReferenceWithNoOut.ts")] // NOT RUNNABLE
#[case("jsFileCompilationErrorOnDeclarationsWithJsFileReferenceWithOut.ts")] // NOT RUNNABLE
#[case("jsFileCompilationErrorOnDeclarationsWithJsFileReferenceWithOutDir.ts")] // NOT RUNNABLE
#[case("jsFileCompilationExportAssignmentSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationExternalPackageError.ts")] // NOT RUNNABLE
#[case("jsFileCompilationHeritageClauseSyntaxOfClass.ts")] // NOT RUNNABLE
#[case("jsFileCompilationImportEqualsSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationInterfaceSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationLetBeingRenamed.ts")] // NOT RUNNABLE
#[case("jsFileCompilationLetDeclarationOrder.ts")] // NOT RUNNABLE
#[case("jsFileCompilationLetDeclarationOrder2.ts")] // NOT RUNNABLE
#[case("jsFileCompilationModuleSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationNoErrorWithoutDeclarationsWithJsFileReferenceWithNoOut.ts")] // NOT RUNNABLE
#[case("jsFileCompilationNoErrorWithoutDeclarationsWithJsFileReferenceWithOut.ts")] // NOT RUNNABLE
#[case("jsFileCompilationNonNullAssertion.ts")] // NOT RUNNABLE
#[case("jsFileCompilationOptionalClassElementSyntaxOfClass.ts")] // NOT RUNNABLE
#[case("jsFileCompilationOptionalParameter.ts")] // NOT RUNNABLE
#[case("jsFileCompilationPublicMethodSyntaxOfClass.ts")] // NOT RUNNABLE
#[case("jsFileCompilationPublicParameterModifier.ts")] // NOT RUNNABLE
#[case("jsFileCompilationRestParamJsDocFunction.ts")] // NOT RUNNABLE
#[case("jsFileCompilationRestParameter.ts")] // NOT RUNNABLE
#[case("jsFileCompilationReturnTypeSyntaxOfFunction.ts")] // NOT RUNNABLE
#[case("jsFileCompilationShortHandProperty.ts")] // NOT RUNNABLE
#[case("jsFileCompilationSyntaxError.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeAliasSyntax.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeArgumentSyntaxOfCall.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeAssertions.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeOfParameter.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeParameterSyntaxOfClass.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeParameterSyntaxOfClassExpression.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeParameterSyntaxOfFunction.ts")] // NOT RUNNABLE
#[case("jsFileCompilationTypeSyntaxOfVar.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithDeclarationEmitPathSameAsInput.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithEnabledCompositeOption.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithJsEmitPathSameAsInput.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithMapFileAsJs.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithMapFileAsJsWithInlineSourceMap.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithMapFileAsJsWithOutDir.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithOut.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithOutDeclarationFileNameSameAsInputJsFile.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithOutFileNameSameAsInputJsFile.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithoutJsExtensions.ts")] // NOT RUNNABLE
#[case("jsFileCompilationWithoutOut.ts")] // NOT RUNNABLE
#[case("jsFileESModuleWithEnumTag.ts")] // NOT RUNNABLE
#[case("jsFileFunctionParametersAsOptional.ts")] // NOT RUNNABLE
#[case("jsFileFunctionParametersAsOptional2.ts")] // NOT RUNNABLE
#[case("jsFileImportPreservedWhenUsed.ts")] // NOT RUNNABLE
#[case("jsFunctionWithPrototypeNoErrorTruncationNoCrash.ts")] // NOT RUNNABLE
#[case("jsNegativeElementAccessNotBound.ts")] // NOT RUNNABLE
#[case("jsNoImplicitAnyNoCascadingReferenceErrors.ts")] // NOT RUNNABLE
#[case("jsPropertyAssignedAfterMethodDeclaration.ts")] // NOT RUNNABLE
#[case("jsPropertyAssignedAfterMethodDeclaration_nonError.ts")] // NOT RUNNABLE
#[case("jsSelfReferencingArgumentsFunction.ts")] // NOT RUNNABLE
#[case("jsdocAccessEnumType.ts")] // NOT RUNNABLE
#[case("jsdocArrayObjectPromiseImplicitAny.ts")] // NOT RUNNABLE
#[case("jsdocArrayObjectPromiseNoImplicitAny.ts")] // NOT RUNNABLE
#[case("jsdocCallbackAndType.ts")] // NOT RUNNABLE
#[case("jsdocCastCommentEmit.ts")]
#[case("jsdocClassMissingTypeArguments.ts")] // NOT RUNNABLE
#[case("jsdocFunctionTypeFalsePositive.ts")] // NOT RUNNABLE
#[case("jsdocIllegalTags.ts")] // NOT RUNNABLE
#[case("jsdocImportTypeNodeNamespace.ts")] // NOT RUNNABLE
#[case("jsdocImportTypeResolution.ts")] // NOT RUNNABLE
#[case("jsdocInTypeScript.ts")]
#[case("jsdocParamTagInvalid.ts")] // NOT RUNNABLE
#[case("jsdocParamTagOnPropertyInitializer.ts")] // NOT RUNNABLE
#[case("jsdocParameterParsingInfiniteLoop.ts")] // NOT RUNNABLE
#[case("jsdocParameterParsingInvalidName.ts")] // NOT RUNNABLE
#[case("jsdocPropertyTagInvalid.ts")] // NOT RUNNABLE
#[case("jsdocReferenceGlobalTypeInCommonJs.ts")] // NOT RUNNABLE
#[case("jsdocResolveNameFailureInTypedef.ts")] // NOT RUNNABLE
#[case("jsdocRestParameter.ts")] // NOT RUNNABLE
#[case("jsdocRestParameter_es6.ts")] // NOT RUNNABLE
#[case("jsdocTypeCast.ts")] // NOT RUNNABLE
#[case("jsdocTypeGenericInstantiationAttempt.ts")] // NOT RUNNABLE
#[case("jsdocTypeNongenericInstantiationAttempt.ts")] // NOT RUNNABLE
#[case("jsdocTypecastNoTypeNoCrash.ts")] // NOT RUNNABLE
#[case("jsdocTypedefBeforeParenthesizedExpression.ts")] // NOT RUNNABLE
#[case("jsdocTypedefMissingType.ts")] // NOT RUNNABLE
#[case("jsdocTypedefNoCrash.ts")] // NOT RUNNABLE
#[case("jsdocTypedefNoCrash2.ts")] // NOT RUNNABLE
#[case("jsdocTypedef_propertyWithNoType.ts")] // NOT RUNNABLE
#[case("jsdocUnexpectedCharacter.ts")] // NOT RUNNABLE
#[case("json.stringify.ts")]
#[case("jsonFileImportChecksCallCorrectlyTwice.ts")] // NOT RUNNABLE
#[case("jsxAttributeMissingInitializer.tsx")]
#[case("jsxAttributeWithoutExpressionReact.tsx")]
#[case("jsxCallElaborationCheckNoCrash1.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxCallbackWithDestructuring.tsx")]
#[case("jsxChildrenGenericContextualTypes.tsx")]
#[case("jsxChildrenIndividualErrorElaborations.tsx")] // NOT RUNNABLE
#[case("jsxChildrenSingleChildConfusableWithMultipleChildrenNoError.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxComplexSignatureHasApplicabilityError.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxComponentTypeErrors.tsx")]
#[case("jsxDeclarationsWithEsModuleInteropNoCrash.tsx")] // NOT RUNNABLE
#[case("jsxElementClassTooManyParams.tsx")]
#[case("jsxEmitAttributeWithPreserve.tsx")]
#[case("jsxEmitWithAttributes.ts")] // NOT RUNNABLE
#[case("jsxEmptyExpressionNotCountedAsChild.tsx")] // NOT RUNNABLE
#[case("jsxExcessPropsAndAssignability.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxFactoryAndJsxFragmentFactory.tsx")]
#[case("jsxFactoryAndJsxFragmentFactoryErrorNotIdentifier.tsx")]
#[case("jsxFactoryAndJsxFragmentFactoryNull.tsx")]
#[case("jsxFactoryAndReactNamespace.ts")] // NOT RUNNABLE
#[case("jsxFactoryButNoJsxFragmentFactory.tsx")]
#[case("jsxFactoryIdentifier.ts")] // NOT RUNNABLE
#[case("jsxFactoryIdentifierAsParameter.ts")] // NOT RUNNABLE
#[case("jsxFactoryIdentifierWithAbsentParameter.ts")] // NOT RUNNABLE
#[case("jsxFactoryMissingErrorInsideAClass.ts")] // NOT RUNNABLE
#[case("jsxFactoryNotIdentifierOrQualifiedName.ts")] // NOT RUNNABLE
#[case("jsxFactoryNotIdentifierOrQualifiedName2.ts")] // NOT RUNNABLE
#[case("jsxFactoryQualifiedName.ts")] // NOT RUNNABLE
#[case("jsxFactoryQualifiedNameResolutionError.ts")] // NOT RUNNABLE
#[case("jsxFactoryQualifiedNameWithEs5.ts")] // NOT RUNNABLE
#[case("jsxFragmentFactoryNoUnusedLocals.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxHasLiteralType.tsx")] // NOT RUNNABLE
#[case("jsxHash.tsx")]
#[case("jsxImportForSideEffectsNonExtantNoError.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxImportInAttribute.tsx")] // NOT RUNNABLE
#[case("jsxImportSourceNonPragmaComment.tsx")]
#[case("jsxInExtendsClause.tsx")]
#[case("jsxInferenceProducesLiteralAsExpected.tsx")] // NOT RUNNABLE
#[case("jsxIntrinsicElementsExtendsRecord.tsx")] // NOT RUNNABLE
#[case("jsxIntrinsicElementsTypeArgumentErrors.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxIntrinsicUnions.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxIssuesErrorWhenTagExpectsTooManyArguments.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxLibraryManagedAttributesUnusedGeneric.tsx")]
#[case("jsxLocalNamespaceIndexSignatureNoCrash.tsx")] // NOT RUNNABLE
#[case("jsxMultilineAttributeStringValues.tsx")]
#[case("jsxMultilineAttributeValuesReact.tsx")]
#[case("jsxNamespaceGlobalReexport.tsx")] // NOT RUNNABLE
#[case("jsxNamespaceGlobalReexportMissingAliasTarget.tsx")] // NOT RUNNABLE
#[case("jsxNamespaceImplicitImportJSXNamespace.tsx")] // NOT RUNNABLE
#[case("jsxNamespaceImplicitImportJSXNamespaceFromConfigPickedOverGlobalOne.tsx")] // NOT RUNNABLE
#[case("jsxNamespaceImplicitImportJSXNamespaceFromPragmaPickedOverGlobalOne.tsx")] // NOT RUNNABLE
#[case("jsxNamespacePrefixInName.tsx")]
#[case("jsxNamespacePrefixInNameReact.tsx")]
#[case("jsxNamespacePrefixIntrinsics.tsx")]
#[case("jsxNamespaceReexports.tsx")] // NOT RUNNABLE
#[case("jsxNestedWithinTernaryParsesCorrectly.tsx")]
#[case("jsxPartialSpread.tsx")] // NOT RUNNABLE on .lib directive
#[case("jsxPreserveWithJsInput.ts")] // NOT RUNNABLE
#[case("jsxPropsAsIdentifierNames.tsx")] // NOT RUNNABLE
#[case("jsxSpreadFirstUnionNoErrors.tsx")] // NOT RUNNABLE
#[case("jsxViaImport.2.tsx")] // NOT RUNNABLE
#[case("jsxViaImport.tsx")] // NOT RUNNABLE
#[case("keepImportsInDts1.ts")] // NOT RUNNABLE
#[case("keepImportsInDts2.ts")] // NOT RUNNABLE
#[case("keepImportsInDts3.ts")] // NOT RUNNABLE
#[case("keepImportsInDts4.ts")] // NOT RUNNABLE
#[case("keyRemappingKeyofResult.ts")]
#[case("keyofDoesntContainSymbols.ts")]
#[case("keyofGenericExtendingClassDoubleLayer.ts")]
#[case("keyofIsLiteralContexualType.ts")]
#[case("keyofModuleObjectHasCorrectKeys.ts")] // NOT RUNNABLE
#[case("keyofObjectWithGlobalSymbolIncluded.ts")]
#[case("keywordExpressionInternalComments.ts")]
#[case("keywordField.ts")]
#[case("keywordInJsxIdentifier.tsx")]
#[case("knockout.ts")]
#[case("lambdaASIEmit.ts")]
#[case("lambdaArgCrash.ts")]
#[case("lambdaExpression.ts")]
#[case("lambdaParamTypes.ts")]
#[case("lambdaParameterWithTupleArgsHasCorrectAssignability.ts")]
#[case("lambdaPropSelf.ts")]
#[case("largeControlFlowGraph.ts")]
#[case("lastPropertyInLiteralWins.ts")]
#[case("lateBoundConstraintTypeChecksCorrectly.ts")]
#[case("lateBoundDestructuringImplicitAnyError.ts")]
#[case("lateBoundFunctionMemberAssignmentDeclarations.ts")] // NOT RUNNABLE
#[case("lateBoundMethodNameAssigmentJS.ts")] // NOT RUNNABLE
#[case("letAndVarRedeclaration.ts")]
#[case("letAsIdentifier.ts")]
#[case("letAsIdentifier2.ts")]
#[case("letAsIdentifierInStrictMode.ts")]
#[case("letConstInCaseClauses.ts")]
#[case("letConstMatchingParameterNames.ts")]
#[case("letDeclarations-access.ts")]
#[case("letDeclarations-es5-1.ts")]
#[case("letDeclarations-es5.ts")]
#[case("letDeclarations-invalidContexts.ts")]
#[case("letDeclarations-scopes-duplicates.ts")]
#[case("letDeclarations-scopes-duplicates2.ts")] // NOT RUNNABLE
#[case("letDeclarations-scopes-duplicates3.ts")] // NOT RUNNABLE
#[case("letDeclarations-scopes-duplicates4.ts")] // NOT RUNNABLE
#[case("letDeclarations-scopes-duplicates5.ts")] // NOT RUNNABLE
#[case("letDeclarations-scopes-duplicates6.ts")] // NOT RUNNABLE
#[case("letDeclarations-scopes-duplicates7.ts")] // NOT RUNNABLE
#[case("letDeclarations-scopes.ts")]
#[case("letDeclarations-scopes2.ts")]
#[case("letDeclarations-useBeforeDefinition.ts")]
#[case("letDeclarations-useBeforeDefinition2.ts")] // NOT RUNNABLE
#[case("letDeclarations-validContexts.ts")]
#[case("letDeclarations.ts")]
#[case("letDeclarations2.ts")]
#[case("letInConstDeclarations_ES5.ts")]
#[case("letInConstDeclarations_ES6.ts")]
#[case("letInLetConstDeclOfForOfAndForIn_ES5.ts")]
#[case("letInLetConstDeclOfForOfAndForIn_ES6.ts")]
#[case("letInLetDeclarations_ES5.ts")]
#[case("letInLetDeclarations_ES6.ts")]
#[case("letInNonStrictMode.ts")]
#[case("letInVarDeclOfForIn_ES5.ts")]
#[case("letInVarDeclOfForIn_ES6.ts")]
#[case("letInVarDeclOfForOf_ES5.ts")]
#[case("letInVarDeclOfForOf_ES6.ts")]
#[case("letKeepNamesOfTopLevelItems.ts")]
#[case("letShadowedByNameInNestedScope.ts")]
#[case("libCompileChecks.ts")]
#[case("libMembers.ts")]
#[case("libTypeScriptOverrideSimple.ts")] // NOT RUNNABLE
#[case("libTypeScriptSubfileResolving.ts")] // NOT RUNNABLE
#[case("libdtsFix.ts")]
#[case("library_ArraySlice.ts")]
#[case("library_DatePrototypeProperties.ts")]
#[case("library_ObjectPrototypeProperties.ts")]
#[case("library_RegExpExecArraySlice.ts")]
#[case("library_StringSlice.ts")]
#[case("lift.ts")]
#[case("limitDeepInstantiations.ts")]
#[case("listFailure.ts")]
#[case("literalFreshnessPropagationOnNarrowing.ts")]
#[case("literalIntersectionYieldsLiteral.ts")]
#[case("literalTypeNameAssertionNotTriggered.ts")] // NOT RUNNABLE
#[case("literals-negative.ts")]
#[case("literals1.ts")]
#[case("literalsInComputedProperties1.ts")]
#[case("localAliasExportAssignment.ts")] // NOT RUNNABLE
#[case("localClassesInLoop.ts")]
#[case("localClassesInLoop_ES6.ts")]
#[case("localImportNameVsGlobalName.ts")]
#[case("localRequireFunction.ts")] // NOT RUNNABLE
#[case("localTypeParameterInferencePriority.ts")]
#[case("localVariablesReturnedFromCatchBlocks.ts")]
#[case("logicalNotExpression1.ts")]
#[case("m7Bugs.ts")]
#[case("manyConstExports.ts")]
#[case("mapConstructorOnReadonlyTuple.ts")]
#[case("mapOnTupleTypes01.ts")]
#[case("mapOnTupleTypes02.ts")]
#[case("mappedToToIndexSignatureInference.ts")]
#[case("mappedTypeAndIndexSignatureRelation.ts")]
#[case("mappedTypeAsStringTemplate.ts")]
#[case("mappedTypeContextualTypesApplied.ts")]
#[case("mappedTypeIndexedAccess.ts")]
#[case("mappedTypeInferenceCircularity.ts")]
#[case("mappedTypeMultiInference.ts")]
#[case("mappedTypeNestedGenericInstantiation.ts")]
#[case("mappedTypeNoTypeNoCrash.ts")] // OUT OF SCOPE transformation error
#[case("mappedTypeParameterConstraint.ts")]
#[case("mappedTypePartialConstraints.ts")]
#[case("mappedTypePartialNonHomomorphicBaseConstraint.ts")]
#[case("mappedTypeRecursiveInference.ts")]
#[case("mappedTypeUnionConstraintInferences.ts")]
#[case("mappedTypeWithAsClauseAndLateBoundProperty.ts")]
#[case("mappedTypeWithAsClauseAndLateBoundProperty2.ts")] // OUT OF SCOPE transformation error
#[case("mappedTypeWithCombinedTypeMappers.ts")]
#[case("matchReturnTypeInAllBranches.ts")]
#[case("matchingOfObjectLiteralConstraints.ts")]
#[case("maxConstraints.ts")]
#[case("maxNodeModuleJsDepthDefaultsToZero.ts")] // NOT RUNNABLE
#[case("maximum10SpellingSuggestions.ts")]
#[case("memberAccessMustUseModuleInstances.ts")] // NOT RUNNABLE
#[case("memberAccessOnConstructorType.ts")]
#[case("memberOverride.ts")]
#[case("memberScope.ts")]
#[case("memberVariableDeclarations1.ts")]
#[case("mergeWithImportedNamespace.ts")] // NOT RUNNABLE
#[case("mergeWithImportedType.ts")] // NOT RUNNABLE
#[case("mergedClassWithNamespacePrototype.ts")] // NOT RUNNABLE
#[case("mergedDeclarationExports.ts")]
#[case("mergedDeclarations1.ts")]
#[case("mergedDeclarations2.ts")]
#[case("mergedDeclarations3.ts")]
#[case("mergedDeclarations4.ts")]
#[case("mergedDeclarations5.ts")] // NOT RUNNABLE
#[case("mergedDeclarations6.ts")] // NOT RUNNABLE
#[case("mergedDeclarations7.ts")] // NOT RUNNABLE
#[case("mergedEnumDeclarationCodeGen.ts")]
#[case("mergedInterfaceFromMultipleFiles1.ts")] // NOT RUNNABLE
#[case("mergedModuleDeclarationCodeGen.ts")]
#[case("mergedModuleDeclarationCodeGen2.ts")]
#[case("mergedModuleDeclarationCodeGen3.ts")]
#[case("mergedModuleDeclarationCodeGen4.ts")]
#[case("mergedModuleDeclarationCodeGen5.ts")]
#[case("mergedModuleDeclarationWithSharedExportedVar.ts")]
#[case("metadataImportType.ts")]
#[case("metadataOfClassFromAlias.ts")] // NOT RUNNABLE
#[case("metadataOfClassFromAlias2.ts")] // NOT RUNNABLE
#[case("metadataOfClassFromModule.ts")]
#[case("metadataOfEventAlias.ts")] // NOT RUNNABLE
#[case("metadataOfStringLiteral.ts")]
#[case("metadataOfUnion.ts")]
#[case("metadataOfUnionWithNull.ts")]
#[case("metadataReferencedWithinFilteredUnion.ts")] // NOT RUNNABLE
#[case("methodChainError.ts")]
#[case("methodContainingLocalFunction.ts")]
#[case("methodInAmbientClass1.ts")]
#[case("methodSignatureDeclarationEmit1.ts")]
#[case("methodSignatureHandledDeclarationKindForSymbol.ts")]
#[case("mismatchedClassConstructorVariable.ts")]
#[case("mismatchedExplicitTypeParameterAndArgumentType.ts")]
#[case("mismatchedGenericArguments1.ts")]
#[case("missingArgument1.ts")]
#[case("missingCloseBrace.ts")]
#[case("missingCloseBraceInObjectLiteral.ts")]
#[case("missingCommaInTemplateStringsArray.ts")]
#[case("missingDomElements.ts")]
#[case("missingFunctionImplementation.ts")]
#[case("missingFunctionImplementation2.ts")] // NOT RUNNABLE
#[case("missingImportAfterModuleImport.ts")] // NOT RUNNABLE
#[case("missingMemberErrorHasShortPath.ts")] // NOT RUNNABLE
#[case("missingPropertiesOfClassExpression.ts")]
#[case("missingRequiredDeclare.d.ts")]
#[case("missingReturnStatement.ts")]
#[case("missingReturnStatement1.ts")]
#[case("missingSelf.ts")]
#[case("missingSemicolonInModuleSpecifier.ts")] // NOT RUNNABLE
#[case("missingTypeArguments1.ts")]
#[case("missingTypeArguments2.ts")]
#[case("missingTypeArguments3.ts")]
#[case("misspelledJsDocTypedefTags.ts")] // NOT RUNNABLE
#[case("misspelledNewMetaProperty.ts")]
#[case("mixedExports.ts")]
#[case("mixedStaticAndInstanceClassMembers.ts")]
#[case("mixinIntersectionIsValidbaseType.ts")]
#[case("mixinOverMappedTypeNoCrash.ts")]
#[case("mixinPrivateAndProtected.ts")]
#[case("mixingApparentTypeOverrides.ts")]
#[case("mixingFunctionAndAmbientModule1.ts")]
#[case("mixingStaticAndInstanceOverloads.ts")]
#[case("modFunctionCrash.ts")]
#[case("modKeyword.ts")]
#[case("modifierOnParameter1.ts")]
#[case("modifiersInObjectLiterals.ts")]
#[case("modifiersOnInterfaceIndexSignature1.ts")]
#[case("modularizeLibrary_Dom.iterable.ts")]
#[case("modularizeLibrary_ErrorFromUsingES6ArrayWithOnlyES6ArrayLib.ts")]
#[case("modularizeLibrary_ErrorFromUsingES6FeaturesWithOnlyES5Lib.ts")]
#[case("modularizeLibrary_ErrorFromUsingWellknownSymbolWithOutES6WellknownSymbolLib.ts")]
#[case("modularizeLibrary_NoErrorDuplicateLibOptions1.ts")]
#[case("modularizeLibrary_NoErrorDuplicateLibOptions2.ts")]
#[case("modularizeLibrary_TargetES5UsingES6Lib.ts")]
#[case("modularizeLibrary_TargetES6UsingES6Lib.ts")]
#[case("modularizeLibrary_UsingES5LibAndES6ArrayLib.ts")]
#[case("modularizeLibrary_UsingES5LibAndES6FeatureLibs.ts")]
#[case("modularizeLibrary_UsingES5LibES6ArrayLibES6WellknownSymbolLib.ts")]
#[case("moduleAliasAsFunctionArgument.ts")] // NOT RUNNABLE
#[case("moduleAliasInterface.ts")]
#[case("moduleAndInterfaceSharingName.ts")]
#[case("moduleAndInterfaceSharingName2.ts")]
#[case("moduleAndInterfaceSharingName3.ts")]
#[case("moduleAndInterfaceSharingName4.ts")]
#[case("moduleAndInterfaceWithSameName.ts")]
#[case("moduleAsBaseType.ts")]
#[case("moduleAssignmentCompat1.ts")]
#[case("moduleAssignmentCompat2.ts")]
#[case("moduleAssignmentCompat3.ts")]
#[case("moduleAssignmentCompat4.ts")]
#[case("moduleAugmentationCollidingNamesInAugmentation1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDeclarationEmit1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDeclarationEmit2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDisallowedExtensions.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDoesInterfaceMergeOfReexport.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDoesNamespaceEnumMergeOfReexport.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDoesNamespaceMergeOfReexport.ts")] // NOT RUNNABLE
#[case("moduleAugmentationDuringSyntheticDefaultCheck.ts")] // NOT RUNNABLE
#[case("moduleAugmentationEnumClassMergeOfReexportIsError.ts")] // NOT RUNNABLE
#[case("moduleAugmentationExtendAmbientModule1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationExtendAmbientModule2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationExtendFileModule1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationExtendFileModule2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationGlobal1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationGlobal2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationGlobal3.ts")] // NOT RUNNABLE
#[case("moduleAugmentationGlobal4.ts")] // NOT RUNNABLE
#[case("moduleAugmentationGlobal5.ts")] // NOT RUNNABLE
#[case("moduleAugmentationGlobal6.ts")]
#[case("moduleAugmentationGlobal6_1.ts")]
#[case("moduleAugmentationGlobal7.ts")]
#[case("moduleAugmentationGlobal7_1.ts")]
#[case("moduleAugmentationGlobal8.ts")]
#[case("moduleAugmentationGlobal8_1.ts")]
#[case("moduleAugmentationImportsAndExports1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationImportsAndExports2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationImportsAndExports3.ts")] // NOT RUNNABLE
#[case("moduleAugmentationImportsAndExports4.ts")] // NOT RUNNABLE
#[case("moduleAugmentationImportsAndExports5.ts")] // NOT RUNNABLE
#[case("moduleAugmentationImportsAndExports6.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInAmbientModule1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInAmbientModule2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInAmbientModule3.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInAmbientModule4.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInAmbientModule5.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInDependency.ts")] // NOT RUNNABLE
#[case("moduleAugmentationInDependency2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationNoNewNames.ts")] // NOT RUNNABLE
#[case("moduleAugmentationOfAlias.ts")] // NOT RUNNABLE
#[case("moduleAugmentationWithNonExistentNamedImport.ts")] // NOT RUNNABLE
#[case("moduleAugmentationsBundledOutput1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationsImports1.ts")] // NOT RUNNABLE
#[case("moduleAugmentationsImports2.ts")] // NOT RUNNABLE
#[case("moduleAugmentationsImports3.ts")] // NOT RUNNABLE
#[case("moduleAugmentationsImports4.ts")] // NOT RUNNABLE
#[case("moduleClassArrayCodeGenTest.ts")]
#[case("moduleCodeGenTest3.ts")]
#[case("moduleCodeGenTest5.ts")]
#[case("moduleCodegenTest4.ts")]
#[case("moduleCrashBug1.ts")]
#[case("moduleDeclarationExportStarShadowingGlobalIsNameable.ts")] // NOT RUNNABLE
#[case("moduleDuplicateIdentifiers.ts")]
#[case("moduleElementsInWrongContext.ts")]
#[case("moduleElementsInWrongContext2.ts")]
#[case("moduleElementsInWrongContext3.ts")]
#[case("moduleExports1.ts")]
#[case("moduleExportsUnaryExpression.ts")]
#[case("moduleIdentifiers.ts")]
#[case("moduleImport.ts")]
#[case("moduleImportedForTypeArgumentPosition.ts")] // NOT RUNNABLE
#[case("moduleInTypePosition1.ts")] // NOT RUNNABLE
#[case("moduleKeywordRepeatError.ts")]
#[case("moduleLocalImportNotIncorrectlyRedirected.ts")] // NOT RUNNABLE
#[case("moduleMemberMissingErrorIsRelative.ts")] // NOT RUNNABLE
#[case("moduleMemberWithoutTypeAnnotation1.ts")]
#[case("moduleMemberWithoutTypeAnnotation2.ts")]
#[case("moduleMerge.ts")]
#[case("moduleMergeConstructor.ts")] // NOT RUNNABLE
#[case("moduleNewExportBug.ts")]
#[case("moduleNoEmit.ts")]
#[case("moduleNoneErrors.ts")] // NOT RUNNABLE
#[case("moduleNoneOutFile.ts")] // NOT RUNNABLE
#[case("moduleOuterQualification.ts")]
#[case("modulePrologueAMD.ts")]
#[case("modulePrologueCommonjs.ts")]
#[case("modulePrologueES6.ts")]
#[case("modulePrologueSystem.ts")]
#[case("modulePrologueUmd.ts")]
#[case("moduleProperty1.ts")]
#[case("moduleProperty2.ts")]
#[case("moduleRedifinitionErrors.ts")]
#[case("moduleReopenedTypeOtherBlock.ts")]
#[case("moduleReopenedTypeSameBlock.ts")]
#[case("moduleResolutionNoResolve.ts")] // NOT RUNNABLE
#[case("moduleResolutionNoTsCJS.ts")] // NOT RUNNABLE
#[case("moduleResolutionNoTsESM.ts")] // NOT RUNNABLE
#[case("moduleResolutionPackageIdWithRelativeAndAbsolutePath.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_notSupported.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_notSupported2.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_notSupported3.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_preferTs.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_unexpected.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_unexpected2.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_withAmbientPresent.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithExtensions_withPaths.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithModule.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithRequire.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithRequireAndImport.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithSymlinks.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithSymlinks_notInNodeModules.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithSymlinks_preserveSymlinks.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithSymlinks_referenceTypes.ts")] // NOT RUNNABLE
#[case("moduleResolutionWithSymlinks_withOutDir.ts")] // NOT RUNNABLE
#[case("moduleResolution_automaticTypeDirectiveNames.ts")] // NOT RUNNABLE
#[case("moduleResolution_explicitNodeModulesImport.ts")] // NOT RUNNABLE
#[case("moduleResolution_explicitNodeModulesImport_implicitAny.ts")] // NOT RUNNABLE
#[case("moduleResolution_noLeadingDot.ts")] // NOT RUNNABLE
#[case("moduleResolution_packageJson_notAtPackageRoot.ts")] // NOT RUNNABLE
#[case("moduleResolution_packageJson_notAtPackageRoot_fakeScopedPackage.ts")] // NOT RUNNABLE
#[case("moduleResolution_packageJson_scopedPackage.ts")] // NOT RUNNABLE
#[case("moduleResolution_packageJson_yesAtPackageRoot.ts")] // NOT RUNNABLE
#[case("moduleResolution_packageJson_yesAtPackageRoot_fakeScopedPackage.ts")] // NOT RUNNABLE
#[case("moduleResolution_packageJson_yesAtPackageRoot_mainFieldInSubDirectory.ts")] // NOT RUNNABLE
#[case("moduleResolution_relativeImportJsFile.ts")] // NOT RUNNABLE
#[case("moduleResolution_relativeImportJsFile_noImplicitAny.ts")] // NOT RUNNABLE
#[case("moduleSameValueDuplicateExportedBindings1.ts")] // NOT RUNNABLE
#[case("moduleSameValueDuplicateExportedBindings2.ts")] // NOT RUNNABLE
#[case("moduleScopingBug.ts")]
#[case("moduleSharesNameWithImportDeclarationInsideIt.ts")]
#[case("moduleSharesNameWithImportDeclarationInsideIt2.ts")]
#[case("moduleSharesNameWithImportDeclarationInsideIt3.ts")]
#[case("moduleSharesNameWithImportDeclarationInsideIt4.ts")]
#[case("moduleSharesNameWithImportDeclarationInsideIt5.ts")]
#[case("moduleSharesNameWithImportDeclarationInsideIt6.ts")]
#[case("moduleSymbolMerging.ts")] // NOT RUNNABLE
#[case("moduleUnassignedVariable.ts")]
#[case("moduleVariableArrayIndexer.ts")]
#[case("moduleVariables.ts")]
#[case("moduleVisibilityTest1.ts")]
#[case("moduleVisibilityTest2.ts")]
#[case("moduleVisibilityTest3.ts")]
#[case("moduleVisibilityTest4.ts")]
#[case("moduleWithNoValuesAsType.ts")]
#[case("moduleWithTryStatement1.ts")]
#[case("moduleWithValuesAsType.ts")]
#[case("module_augmentExistingAmbientVariable.ts")]
#[case("module_augmentExistingVariable.ts")]
#[case("module_augmentUninstantiatedModule.ts")]
#[case("module_augmentUninstantiatedModule2.ts")] // NOT RUNNABLE
#[case("moduledecl.ts")]
#[case("multiCallOverloads.ts")]
#[case("multiExtendsSplitInterfaces1.ts")]
#[case("multiExtendsSplitInterfaces2.ts")]
#[case("multiImportExport.ts")] // NOT RUNNABLE
#[case("multiLineContextDiagnosticWithPretty.ts")] // FAILING 3702 on pretty-printing looks right just failing on massaging line numbers
#[case("multiLineErrors.ts")]
#[case("multiLinePropertyAccessAndArrowFunctionIndent1.ts")]
#[case("multiModuleClodule1.ts")]
#[case("multiModuleFundule1.ts")]
#[case("multipleBaseInterfaesWithIncompatibleProperties.ts")]
#[case("multipleClassPropertyModifiers.ts")]
#[case("multipleClassPropertyModifiersErrors.ts")]
#[case("multipleExportAssignments.ts")]
#[case("multipleExportAssignmentsInAmbientDeclaration.ts")]
#[case("multipleExports.ts")]
#[case("multipleInheritance.ts")]
#[case("multivar.ts")]
#[case("mutrec.ts")]
#[case("mutuallyRecursiveCallbacks.ts")]
#[case("mutuallyRecursiveGenericBaseTypes1.ts")]
#[case("mutuallyRecursiveGenericBaseTypes2.ts")]
#[case("mutuallyRecursiveInference.ts")]
#[case("mutuallyRecursiveInterfaceDeclaration.ts")]
#[case("nameCollisionWithBlockScopedVariable1.ts")]
#[case("nameCollisions.ts")]
#[case("nameCollisionsInPropertyAssignments.ts")]
#[case("namedFunctionExpressionAssignedToClassProperty.ts")]
#[case("namedFunctionExpressionCall.ts")]
#[case("namedFunctionExpressionCallErrors.ts")]
#[case("namedFunctionExpressionInModule.ts")]
#[case("namedImportNonExistentName.ts")] // NOT RUNNABLE
#[case("namespaceDisambiguationInUnion.ts")]
#[case("namespaceMergedWithFunctionWithOverloadsUsage.ts")] // NOT RUNNABLE
#[case("namespaceMergedWithImportAliasNoCrash.ts")] // NOT RUNNABLE
#[case("namespaces1.ts")]
#[case("namespaces2.ts")]
#[case("namespacesDeclaration1.ts")]
#[case("namespacesDeclaration2.ts")]
#[case("namespacesWithTypeAliasOnlyExportsMerge.ts")] // NOT RUNNABLE
#[case("narrowByEquality.ts")]
#[case("narrowCommaOperatorNestedWithinLHS.ts")]
#[case("narrowTypeByInstanceof.ts")]
#[case("narrowUnknownByTypeofObject.ts")]
#[case("narrowedConstInMethod.ts")]
#[case("narrowedImports.ts")] // NOT RUNNABLE
#[case("narrowedImports_assumeInitialized.ts")] // NOT RUNNABLE
#[case("narrowingAssignmentReadonlyRespectsAssertion.ts")]
#[case("narrowingByDiscriminantInLoop.ts")]
#[case("narrowingByTypeofInSwitch.ts")]
#[case("narrowingConstrainedTypeParameter.ts")]
#[case("narrowingIntersection.ts")]
#[case("narrowingOfDottedNames.ts")]
#[case("narrowingOfQualifiedNames.ts")]
#[case("narrowingOrderIndependent.ts")]
#[case("narrowingTruthyObject.ts")]
#[case("narrowingWithNonNullExpression.ts")]
#[case("nativeToBoxedTypes.ts")]
#[case("nearbyIdenticalGenericLambdasAssignable.ts")]
#[case("negativeZero.ts")]
#[case("nestedBlockScopedBindings1.ts")]
#[case("nestedBlockScopedBindings10.ts")]
#[case("nestedBlockScopedBindings11.ts")]
#[case("nestedBlockScopedBindings12.ts")]
#[case("nestedBlockScopedBindings13.ts")]
#[case("nestedBlockScopedBindings14.ts")]
#[case("nestedBlockScopedBindings15.ts")]
#[case("nestedBlockScopedBindings16.ts")]
#[case("nestedBlockScopedBindings2.ts")]
#[case("nestedBlockScopedBindings3.ts")]
#[case("nestedBlockScopedBindings4.ts")]
#[case("nestedBlockScopedBindings5.ts")]
#[case("nestedBlockScopedBindings6.ts")]
#[case("nestedBlockScopedBindings7.ts")]
#[case("nestedBlockScopedBindings8.ts")]
#[case("nestedBlockScopedBindings9.ts")]
#[case("nestedCallbackErrorNotFlattened.ts")]
#[case("nestedFreshLiteral.ts")]
#[case("nestedGenericConditionalTypeWithGenericImportType.ts")] // NOT RUNNABLE
#[case("nestedGenerics.ts")]
#[case("nestedGlobalNamespaceInClass.ts")]
#[case("nestedIfStatement.ts")]
#[case("nestedIndexer.ts")]
#[case("nestedInfinitelyExpandedRecursiveTypes.ts")]
#[case("nestedLoopTypeGuards.ts")]
#[case("nestedLoopWithOnlyInnerLetCaptured.ts")]
#[case("nestedLoops.ts")]
#[case("nestedModulePrivateAccess.ts")]
#[case("nestedRecursiveArraysOrObjectsError01.ts")]
#[case("nestedRecursiveLambda.ts")]
#[case("nestedRedeclarationInES6AMD.ts")]
#[case("nestedSelf.ts")]
#[case("nestedThisContainer.ts")]
#[case("nestedTypeVariableInfersLiteral.ts")]
#[case("newAbstractInstance.ts")]
#[case("newAbstractInstance2.ts")] // NOT RUNNABLE
#[case("newArrays.ts")]
#[case("newExpressionWithCast.ts")]
#[case("newExpressionWithTypeParameterConstrainedToOuterTypeParameter.ts")]
#[case("newFunctionImplicitAny.ts")]
#[case("newLexicalEnvironmentForConvertedLoop.ts")]
#[case("newLineFlagWithCRLF.ts")] // NOT RUNNABLE
#[case("newLineFlagWithLF.ts")] // NOT RUNNABLE
#[case("newMap.ts")]
#[case("newMissingIdentifier.ts")]
#[case("newNamesInGlobalAugmentations1.ts")] // NOT RUNNABLE
#[case("newNonReferenceType.ts")]
#[case("newOnInstanceSymbol.ts")]
#[case("newOperator.ts")]
#[case("noAsConstNameLookup.ts")]
#[case("noBundledEmitFromNodeModules.ts")] // NOT RUNNABLE
#[case("noCatchBlock.ts")]
#[case("noCircularDefinitionOnExportOfPrivateInMergedNamespace.ts")]
#[case("noCollisionThisExpressionAndClassInGlobal.ts")]
#[case("noCollisionThisExpressionAndLocalVarInAccessors.ts")]
#[case("noCollisionThisExpressionAndLocalVarInConstructor.ts")]
#[case("noCollisionThisExpressionAndLocalVarInFunction.ts")]
#[case("noCollisionThisExpressionAndLocalVarInLambda.ts")]
#[case("noCollisionThisExpressionAndLocalVarInMethod.ts")]
#[case("noCollisionThisExpressionAndLocalVarInProperty.ts")]
#[case("noCollisionThisExpressionAndVarInGlobal.ts")]
#[case("noCollisionThisExpressionInFunctionAndVarInGlobal.ts")]
#[case("noConstraintInReturnType1.ts")]
#[case("noCrashOnImportShadowing.ts")] // NOT RUNNABLE
#[case("noCrashOnMixin.ts")]
#[case("noCrashOnNoLib.ts")]
#[case("noCrashOnParameterNamedRequire.ts")] // NOT RUNNABLE
#[case("noCrashOnThisTypeUsage.ts")]
#[case("noCrashUMDMergedWithGlobalValue.ts")] // NOT RUNNABLE
#[case("noDefaultLib.ts")]
#[case("noEmitAndComposite.ts")] // NOT RUNNABLE
#[case("noEmitAndCompositeListFilesOnly.ts")] // NOT RUNNABLE
#[case("noEmitAndIncremental.ts")] // NOT RUNNABLE
#[case("noEmitAndIncrementalListFilesOnly.ts")] // NOT RUNNABLE
#[case("noEmitHelpers.ts")]
#[case("noEmitHelpers2.ts")]
#[case("noEmitOnError.ts")]
#[case("noErrorTruncation.ts")]
#[case("noErrorUsingImportExportModuleAugmentationInDeclarationFile1.ts")] // NOT RUNNABLE
#[case("noErrorUsingImportExportModuleAugmentationInDeclarationFile2.ts")] // NOT RUNNABLE
#[case("noErrorUsingImportExportModuleAugmentationInDeclarationFile3.ts")] // NOT RUNNABLE
#[case("noErrorsInCallback.ts")]
#[case("noExcessiveStackDepthError.ts")]
#[case("noImplicitAnyAndPrivateMembersWithoutTypeAnnotations.ts")] // NOT RUNNABLE
#[case("noImplicitAnyDestructuringInPrivateMethod.ts")]
#[case("noImplicitAnyDestructuringParameterDeclaration.ts")]
#[case("noImplicitAnyDestructuringVarDeclaration.ts")]
#[case("noImplicitAnyDestructuringVarDeclaration2.ts")]
#[case("noImplicitAnyForIn.ts")]
#[case("noImplicitAnyForMethodParameters.ts")]
#[case("noImplicitAnyForwardReferencedInterface.ts")]
#[case("noImplicitAnyFunctionExpressionAssignment.ts")]
#[case("noImplicitAnyFunctions.ts")]
#[case("noImplicitAnyInBareInterface.ts")]
#[case("noImplicitAnyInCastExpression.ts")]
#[case("noImplicitAnyInContextuallyTypesFunctionParamter.ts")]
#[case("noImplicitAnyIndexing.ts")]
#[case("noImplicitAnyIndexingSuppressed.ts")]
#[case("noImplicitAnyLoopCrash.ts")]
#[case("noImplicitAnyMissingGetAccessor.ts")]
#[case("noImplicitAnyMissingSetAccessor.ts")]
#[case("noImplicitAnyModule.ts")]
#[case("noImplicitAnyNamelessParameter.ts")]
#[case("noImplicitAnyParametersInAmbientClass.ts")]
#[case("noImplicitAnyParametersInAmbientFunctions.ts")]
#[case("noImplicitAnyParametersInAmbientModule.ts")]
#[case("noImplicitAnyParametersInBareFunctions.ts")]
#[case("noImplicitAnyParametersInClass.ts")]
#[case("noImplicitAnyParametersInInterface.ts")]
#[case("noImplicitAnyParametersInModule.ts")]
#[case("noImplicitAnyReferencingDeclaredInterface.ts")]
#[case("noImplicitAnyStringIndexerOnObject.ts")]
#[case("noImplicitAnyWithOverloads.ts")]
#[case("noImplicitReturnInConstructors.ts")]
#[case("noImplicitReturnsInAsync1.ts")]
#[case("noImplicitReturnsInAsync2.ts")]
#[case("noImplicitReturnsWithProtectedBlocks1.ts")]
#[case("noImplicitReturnsWithProtectedBlocks2.ts")]
#[case("noImplicitReturnsWithProtectedBlocks3.ts")]
#[case("noImplicitReturnsWithoutReturnExpression.ts")]
#[case("noImplicitSymbolToString.ts")]
#[case("noImplicitThisBigThis.ts")]
#[case("noImplicitThisFunctions.ts")]
#[case("noImplicitUseStrict_amd.ts")]
#[case("noImplicitUseStrict_commonjs.ts")]
#[case("noImplicitUseStrict_es6.ts")]
#[case("noImplicitUseStrict_system.ts")]
#[case("noImplicitUseStrict_umd.ts")]
#[case("noIterationTypeErrorsInCFA.ts")]
#[case("noObjectKeysToKeyofT.ts")]
#[case("noReachabilityErrorsOnEmptyStatement.ts")]
#[case("noSelfOnVars.ts")]
#[case("noStrictGenericChecks.ts")]
#[case("noSubstitutionTemplateStringLiteralTypes.ts")]
#[case("noSymbolForMergeCrash.ts")] // NOT RUNNABLE
#[case("noTypeArgumentOnReturnType1.ts")]
#[case("noUnusedLocals_destructuringAssignment.ts")]
#[case("noUnusedLocals_selfReference.ts")]
#[case("noUnusedLocals_selfReference_skipsBlockLocations.ts")]
#[case("noUnusedLocals_typeParameterMergedWithParameter.ts")]
#[case("noUnusedLocals_writeOnly.ts")]
#[case("noUnusedLocals_writeOnlyProperty.ts")]
#[case("noUnusedLocals_writeOnlyProperty_dynamicNames.ts")]
#[case("noUsedBeforeDefinedErrorInAmbientContext1.ts")] // NOT RUNNABLE
#[case("nodeModuleReexportFromDottedPath.ts")] // NOT RUNNABLE
#[case("nodeResolution1.ts")] // NOT RUNNABLE
#[case("nodeResolution2.ts")] // NOT RUNNABLE
#[case("nodeResolution3.ts")] // NOT RUNNABLE
#[case("nodeResolution4.ts")] // NOT RUNNABLE
#[case("nodeResolution5.ts")] // NOT RUNNABLE
#[case("nodeResolution6.ts")] // NOT RUNNABLE
#[case("nodeResolution7.ts")] // NOT RUNNABLE
#[case("nodeResolution8.ts")] // NOT RUNNABLE
#[case("nonArrayRestArgs.ts")]
#[case("nonConflictingRecursiveBaseTypeMembers.ts")]
#[case("nonContextuallyTypedLogicalOr.ts")]
#[case("nonExportedElementsOfMergedModules.ts")]
#[case("nonGenericClassExtendingGenericClassWithAny.ts")]
#[case("nonIdenticalTypeConstraints.ts")]
#[case("nonMergedDeclarationsAndOverloads.ts")]
#[case("nonMergedOverloads.ts")]
#[case("nonNullMappedType.ts")]
#[case("nonNullParameterExtendingStringAssignableToString.ts")]
#[case("nonNullReferenceMatching.ts")]
#[case("nonNullableReduction.ts")]
#[case("nonNullableReductionNonStrict.ts")]
#[case("nonObjectUnionNestedExcessPropertyCheck.ts")]
#[case("nondistributiveConditionalTypeInfer.ts")]
#[case("nonexistentPropertyOnUnion.ts")]
#[case("nonexistentPropertyUnavailableOnPromisedType.ts")]
#[case("nongenericConditionalNotPartiallyComputed.ts")]
#[case("nongenericPartialInstantiationsRelatedInBothDirections.ts")]
#[case("nonnullAssertionPropegatesContextualType.ts")]
#[case("nonstrictTemplateWithNotOctalPrintsAsIs.ts")]
#[case("normalizedIntersectionTooComplex.ts")]
#[case("nounusedTypeParameterConstraint.ts")] // NOT RUNNABLE
#[case("null.ts")]
#[case("nullKeyword.ts")]
#[case("nullableFunctionError.ts")]
#[case("numberAsInLHS.ts")]
#[case("numberAssignableToEnumInsideUnion.ts")]
#[case("numberFormatCurrencySign.ts")]
#[case("numberOnLeftSideOfInExpression.ts")]
#[case("numberToString.ts")]
#[case("numberVsBigIntOperations.ts")]
#[case("numericClassMembers1.ts")]
#[case("numericEnumMappedType.ts")]
#[case("numericIndexExpressions.ts")]
#[case("numericIndexerConstraint.ts")]
#[case("numericIndexerConstraint1.ts")]
#[case("numericIndexerConstraint2.ts")]
#[case("numericIndexerConstraint3.ts")]
#[case("numericIndexerConstraint4.ts")]
#[case("numericIndexerConstraint5.ts")]
#[case("numericIndexerTyping1.ts")]
#[case("numericIndexerTyping2.ts")]
#[case("numericLiteralsWithTrailingDecimalPoints01.ts")]
#[case("numericLiteralsWithTrailingDecimalPoints02.ts")]
#[case("numericMethodName1.ts")]
#[case("numericUnderscoredSeparator.ts")] // NOT RUNNABLE
#[case("objectBindingPattern_restElementWithPropertyName.ts")]
#[case("objectCreate-errors.ts")]
#[case("objectCreate.ts")]
#[case("objectCreate2.ts")]
#[case("objectCreationExpressionInFunctionParameter.ts")]
#[case("objectCreationOfElementAccessExpression.ts")]
#[case("objectFreeze.ts")]
#[case("objectFromEntries.ts")]
#[case("objectIndexer.ts")]
#[case("objectInstantiationFromUnionSpread.ts")]
#[case("objectLitArrayDeclNoNew.ts")]
#[case("objectLitGetterSetter.ts")]
#[case("objectLitIndexerContextualType.ts")]
#[case("objectLitPropertyScoping.ts")]
#[case("objectLitStructuralTypeMismatch.ts")]
#[case("objectLitTargetTypeCallSite.ts")]
#[case("objectLiteral1.ts")]
#[case("objectLiteral2.ts")]
#[case("objectLiteralArraySpecialization.ts")]
#[case("objectLiteralComputedNameNoDeclarationError.ts")]
#[case("objectLiteralDeclarationGeneration1.ts")]
#[case("objectLiteralEnumPropertyNames.ts")]
#[case("objectLiteralExcessProperties.ts")]
#[case("objectLiteralFreshnessWithSpread.ts")]
#[case("objectLiteralFunctionArgContextualTyping.ts")]
#[case("objectLiteralFunctionArgContextualTyping2.ts")]
#[case("objectLiteralIndexerErrors.ts")]
#[case("objectLiteralIndexerNoImplicitAny.ts")]
#[case("objectLiteralIndexers.ts")]
#[case("objectLiteralMemberWithModifiers1.ts")]
#[case("objectLiteralMemberWithModifiers2.ts")]
#[case("objectLiteralMemberWithQuestionMark1.ts")]
#[case("objectLiteralMemberWithoutBlock1.ts")]
#[case("objectLiteralParameterResolution.ts")]
#[case("objectLiteralPropertyImplicitlyAny.ts")]
#[case("objectLiteralReferencingInternalProperties.ts")]
#[case("objectLiteralThisWidenedOnUse.ts")]
#[case("objectLiteralWithGetAccessorInsideFunction.ts")]
#[case("objectLiteralWithNumericPropertyName.ts")]
#[case("objectLiteralWithSemicolons1.ts")]
#[case("objectLiteralWithSemicolons2.ts")]
#[case("objectLiteralWithSemicolons3.ts")]
#[case("objectLiteralWithSemicolons4.ts")]
#[case("objectLiteralWithSemicolons5.ts")]
#[case("objectLiteralsAgainstUnionsOfArrays01.ts")]
#[case("objectMembersOnTypes.ts")]
#[case("objectRestSpread.ts")] // NOT RUNNABLE
#[case("objectSpreadWithinMethodWithinObjectWithSpread.ts")]
#[case("objectTypeWithOptionalProperty1.ts")]
#[case("observableInferenceCanBeMade.ts")]
#[case("omitTypeHelperModifiers01.ts")]
#[case("omitTypeTestErrors01.ts")]
#[case("omitTypeTests01.ts")]
#[case("operationsAvailableOnPromisedType.ts")]
#[case("operatorAddNullUndefined.ts")]
#[case("optionalAccessorsInInterface1.ts")]
#[case("optionalArgsWithDefaultValues.ts")]
#[case("optionalConstructorArgInSuper.ts")]
#[case("optionalFunctionArgAssignability.ts")]
#[case("optionalParamArgsTest.ts")]
#[case("optionalParamAssignmentCompat.ts")]
#[case("optionalParamInOverride.ts")]
#[case("optionalParamReferencingOtherParams1.ts")]
#[case("optionalParamReferencingOtherParams2.ts")]
#[case("optionalParamReferencingOtherParams3.ts")]
#[case("optionalParamTypeComparison.ts")]
#[case("optionalParameterInDestructuringWithInitializer.ts")]
#[case("optionalParameterProperty.ts")]
#[case("optionalParameterRetainsNull.ts")]
#[case("optionalParamterAndVariableDeclaration.ts")]
#[case("optionalParamterAndVariableDeclaration2.ts")]
#[case("optionalPropertiesInClasses.ts")]
#[case("optionalPropertiesSyntax.ts")]
#[case("optionalPropertiesTest.ts")]
#[case("optionalSetterParam.ts")]
#[case("optionsCompositeWithIncrementalFalse.ts")]
#[case("optionsInlineSourceMapMapRoot.ts")]
#[case("optionsInlineSourceMapSourceRoot.ts")]
#[case("optionsInlineSourceMapSourcemap.ts")]
#[case("optionsOutAndNoModuleGen.ts")]
#[case("optionsSourcemapInlineSources.ts")]
#[case("optionsSourcemapInlineSourcesMapRoot.ts")]
#[case("optionsSourcemapInlineSourcesSourceRoot.ts")]
#[case("optionsStrictPropertyInitializationStrict.ts")]
#[case("optionsStrictPropertyInitializationStrictNullChecks.ts")]
#[case("optionsTsBuildInfoFileWithoutIncrementalAndComposite.ts")]
#[case("orderMattersForSignatureGroupIdentity.ts")]
#[case("out-flag.ts")]
#[case("out-flag2.ts")] // NOT RUNNABLE
#[case("out-flag3.ts")] // NOT RUNNABLE
#[case("outModuleConcatAmd.ts")] // NOT RUNNABLE
#[case("outModuleConcatCommonjs.ts")] // NOT RUNNABLE
#[case("outModuleConcatCommonjsDeclarationOnly.ts")] // NOT RUNNABLE
#[case("outModuleConcatES6.ts")] // NOT RUNNABLE
#[case("outModuleConcatSystem.ts")] // NOT RUNNABLE
#[case("outModuleConcatUmd.ts")] // NOT RUNNABLE
#[case("outModuleConcatUnspecifiedModuleKind.ts")] // NOT RUNNABLE
#[case("outModuleConcatUnspecifiedModuleKindDeclarationOnly.ts")] // NOT RUNNABLE
#[case("outModuleTripleSlashRefs.ts")] // NOT RUNNABLE
#[case("overEagerReturnTypeSpecialization.ts")]
#[case("overload1.ts")]
#[case("overload2.ts")]
#[case("overloadAssignmentCompat.ts")]
#[case("overloadBindingAcrossDeclarationBoundaries.ts")]
#[case("overloadBindingAcrossDeclarationBoundaries2.ts")] // NOT RUNNABLE
#[case("overloadCallTest.ts")]
#[case("overloadConsecutiveness.ts")]
#[case("overloadCrash.ts")]
#[case("overloadEquivalenceWithStatics.ts")]
#[case("overloadErrorMatchesImplementationElaboaration.ts")]
#[case("overloadGenericFunctionWithRestArgs.ts")]
#[case("overloadModifiersMustAgree.ts")]
#[case("overloadOnConstAsTypeAnnotation.ts")]
#[case("overloadOnConstConstraintChecks1.ts")]
#[case("overloadOnConstConstraintChecks2.ts")]
#[case("overloadOnConstConstraintChecks3.ts")]
#[case("overloadOnConstConstraintChecks4.ts")]
#[case("overloadOnConstDuplicateOverloads1.ts")]
#[case("overloadOnConstInBaseWithBadImplementationInDerived.ts")]
#[case("overloadOnConstInCallback1.ts")]
#[case("overloadOnConstInObjectLiteralImplementingAnInterface.ts")]
#[case("overloadOnConstInheritance1.ts")]
#[case("overloadOnConstInheritance2.ts")]
#[case("overloadOnConstInheritance3.ts")]
#[case("overloadOnConstInheritance4.ts")]
#[case("overloadOnConstNoAnyImplementation.ts")]
#[case("overloadOnConstNoAnyImplementation2.ts")]
#[case("overloadOnConstNoNonSpecializedSignature.ts")]
#[case("overloadOnConstNoStringImplementation.ts")]
#[case("overloadOnConstNoStringImplementation2.ts")]
#[case("overloadOnConstantsInvalidOverload1.ts")]
#[case("overloadOnGenericArity.ts")]
#[case("overloadOnGenericClassAndNonGenericClass.ts")]
#[case("overloadResolutionOnDefaultConstructor1.ts")]
#[case("overloadResolutionOverCTLambda.ts")]
#[case("overloadResolutionOverNonCTLambdas.ts")]
#[case("overloadResolutionOverNonCTObjectLit.ts")]
#[case("overloadResolutionTest1.ts")]
#[case("overloadResolutionWithAny.ts")]
#[case("overloadRet.ts")]
#[case("overloadReturnTypes.ts")]
#[case("overloadWithCallbacksWithDifferingOptionalityOnArgs.ts")]
#[case("overloadedConstructorFixesInferencesAppropriately.ts")]
#[case("overloadedStaticMethodSpecialization.ts")]
#[case("overloadingOnConstants1.ts")]
#[case("overloadingOnConstants2.ts")]
#[case("overloadingOnConstantsInImplementation.ts")]
#[case("overloadingStaticFunctionsInFunctions.ts")]
#[case("overloadresolutionWithConstraintCheckingDeferred.ts")]
#[case("overloadsAndTypeArgumentArity.ts")]
#[case("overloadsAndTypeArgumentArityErrors.ts")]
#[case("overloadsInDifferentContainersDisagreeOnAmbient.ts")]
#[case("overloadsWithConstraints.ts")]
#[case("overloadsWithProvisionalErrors.ts")]
#[case("overloadsWithinClasses.ts")]
#[case("overrideBaseIntersectionMethod.ts")]
#[case("overridingPrivateStaticMembers.ts")]
#[case("paramPropertiesInSignatures.ts")]
#[case("parameterDestructuringObjectLiteral.ts")]
#[case("parameterInitializerBeforeDestructuringEmit.ts")]
#[case("parameterListAsTupleType.ts")]
#[case("parameterNamesInTypeParameterList.ts")]
#[case("parameterPropertyInConstructor1.ts")]
#[case("parameterPropertyInConstructor2.ts")]
#[case("parameterPropertyInConstructor3.ts")]
#[case("parameterPropertyInitializerInInitializers.ts")]
#[case("parameterPropertyOutsideConstructor.ts")]
#[case("parameterPropertyReferencingOtherParameter.ts")]
#[case("parameterReferenceInInitializer1.ts")]
#[case("parameterReferenceInInitializer2.ts")]
#[case("parameterReferencesOtherParameter1.ts")]
#[case("parameterReferencesOtherParameter2.ts")]
#[case("paramsOnlyHaveLiteralTypesWhenAppropriatelyContextualized.ts")]
#[case("paramterDestrcuturingDeclaration.ts")]
#[case("parenthesisDoesNotBlockAliasSymbolCreation.ts")]
#[case("parenthesizedArrowExpressionASI.ts")]
#[case("parenthesizedAsyncArrowFunction.ts")]
#[case("parenthesizedExpressionInternalComments.ts")]
#[case("parse1.ts")]
#[case("parse2.ts")]
#[case("parseArrowFunctionWithFunctionReturnType.ts")]
#[case("parseBigInt.ts")] // FAILING 4138 on BigInt parsing
#[case("parseCommaSeparatedNewlineNew.ts")]
#[case("parseCommaSeparatedNewlineNumber.ts")]
#[case("parseCommaSeparatedNewlineString.ts")]
#[case("parseEntityNameWithReservedWord.ts")]
#[case("parseErrorDoubleCommaInCall.ts")]
#[case("parseErrorInHeritageClause1.ts")]
#[case("parseErrorIncorrectReturnToken.ts")]
#[case("parseGenericArrowRatherThanLeftShift.ts")]
#[case("parseInvalidNames.ts")]
#[case("parseObjectLiteralsWithoutTypes.ts")]
#[case("parseShortform.ts")]
#[case("parseTypes.ts")]
#[case("parserConstructorDeclaration12.ts")]
#[case("parserIsClassMemberStart.ts")]
#[case("parsingClassRecoversWhenHittingUnexpectedSemicolon.ts")]
#[case("parsingDeepParenthensizedExpression.ts")] // NOT RUNNABLE
#[case("partialDiscriminatedUnionMemberHasGoodError.ts")]
#[case("partialOfLargeAPIIsAbleToBeWorkedWith.ts")]
#[case("partialTypeNarrowedToByTypeGuard.ts")]
#[case("partiallyAmbientClodule.ts")]
#[case("partiallyAmbientFundule.ts")]
#[case("partiallyDiscriminantedUnions.ts")]
#[case("pathMappingBasedModuleResolution1_amd.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution1_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution2_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution2_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution3_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution3_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution4_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution4_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution5_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution5_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution6_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution6_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution7_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution7_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution8_classic.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution8_node.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_rootImport_aliasWithRoot.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_rootImport_aliasWithRoot_differentRootTypes.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_rootImport_aliasWithRoot_multipleAliases.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_rootImport_aliasWithRoot_realRootFile.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_rootImport_noAliasWithRoot.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_rootImport_noAliasWithRoot_realRootFile.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_withExtension.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_withExtensionInName.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_withExtension_MapedToNodeModules.ts")] // NOT RUNNABLE
#[case("pathMappingBasedModuleResolution_withExtension_failedLookup.ts")] // NOT RUNNABLE
#[case("pathMappingInheritedBaseUrl.ts")] // NOT RUNNABLE
#[case("pathMappingWithoutBaseUrl1.ts")] // NOT RUNNABLE
#[case("pathMappingWithoutBaseUrl2.ts")] // NOT RUNNABLE
#[case("pathsValidation1.ts")] // NOT RUNNABLE
#[case("pathsValidation2.ts")] // NOT RUNNABLE
#[case("pathsValidation3.ts")] // NOT RUNNABLE
#[case("pathsValidation4.ts")] // NOT RUNNABLE
#[case("pathsValidation5.ts")] // NOT RUNNABLE
#[case("performanceComparisonOfStructurallyIdenticalInterfacesWithGenericSignatures.ts")]
#[case("pinnedComments1.ts")]
#[case("potentiallyUncalledDecorators.ts")]
#[case("prefixIncrementAsOperandOfPlusExpression.ts")]
#[case("prefixUnaryOperatorsOnExportedVariables.ts")]
#[case("prefixedNumberLiteralAssignToNumberLiteralType.ts")]
#[case("preserveConstEnums.ts")] // FAILING 4201 on Number stuff
#[case("preserveUnusedImports.ts")] // NOT RUNNABLE
#[case("prespecializedGenericMembers1.ts")]
#[case("prettyContextNotDebugAssertion.ts")] // NOT RUNNABLE
#[case("prettyFileWithErrorsAndTabs.ts")] // FAILING 4205 on pretty-printing
#[case("primaryExpressionMods.ts")]
#[case("primitiveConstraints1.ts")]
#[case("primitiveConstraints2.ts")]
#[case("primitiveMembers.ts")]
#[case("primitiveTypeAsClassName.ts")]
#[case("primitiveTypeAsInterfaceName.ts")]
#[case("primitiveTypeAsInterfaceNameGeneric.ts")]
#[case("primitiveTypeAsmoduleName.ts")]
#[case("primitiveTypeAssignment.ts")]
#[case("primitiveUnionDetection.ts")]
#[case("privacyAccessorDeclFile.ts")] // NOT RUNNABLE
#[case("privacyCannotNameAccessorDeclFile.ts")] // NOT RUNNABLE
#[case("privacyCannotNameVarTypeDeclFile.ts")] // NOT RUNNABLE
#[case("privacyCheckAnonymousFunctionParameter.ts")]
#[case("privacyCheckAnonymousFunctionParameter2.ts")]
#[case("privacyCheckCallbackOfInterfaceMethodWithTypeParameter.ts")]
#[case("privacyCheckExportAssignmentOnExportedGenericInterface1.ts")]
#[case("privacyCheckExportAssignmentOnExportedGenericInterface2.ts")]
#[case("privacyCheckExternalModuleExportAssignmentOfGenericClass.ts")] // NOT RUNNABLE
#[case("privacyCheckOnTypeParameterReferenceInConstructorParameter.ts")]
#[case("privacyCheckTypeOfFunction.ts")]
#[case("privacyCheckTypeOfInvisibleModuleError.ts")]
#[case("privacyCheckTypeOfInvisibleModuleNoError.ts")]
#[case("privacyClass.ts")]
#[case("privacyClassExtendsClauseDeclFile.ts")] // NOT RUNNABLE
#[case("privacyClassImplementsClauseDeclFile.ts")] // NOT RUNNABLE
#[case("privacyFunc.ts")]
#[case("privacyFunctionCannotNameParameterTypeDeclFile.ts")] // NOT RUNNABLE
#[case("privacyFunctionCannotNameReturnTypeDeclFile.ts")] // NOT RUNNABLE
#[case("privacyFunctionParameterDeclFile.ts")] // NOT RUNNABLE
#[case("privacyFunctionReturnTypeDeclFile.ts")] // NOT RUNNABLE
#[case("privacyGetter.ts")]
#[case("privacyGloClass.ts")]
#[case("privacyGloFunc.ts")]
#[case("privacyGloGetter.ts")]
#[case("privacyGloImport.ts")]
#[case("privacyGloImportParseErrors.ts")]
#[case("privacyGloInterface.ts")]
#[case("privacyGloVar.ts")]
#[case("privacyImport.ts")]
#[case("privacyImportParseErrors.ts")]
#[case("privacyInterface.ts")]
#[case("privacyInterfaceExtendsClauseDeclFile.ts")] // NOT RUNNABLE
#[case("privacyLocalInternalReferenceImportWithExport.ts")]
#[case("privacyLocalInternalReferenceImportWithoutExport.ts")]
#[case("privacyTopLevelAmbientExternalModuleImportWithExport.ts")] // NOT RUNNABLE
#[case("privacyTopLevelAmbientExternalModuleImportWithoutExport.ts")] // NOT RUNNABLE
#[case("privacyTopLevelInternalReferenceImportWithExport.ts")]
#[case("privacyTopLevelInternalReferenceImportWithoutExport.ts")]
#[case("privacyTypeParameterOfFunction.ts")]
#[case("privacyTypeParameterOfFunctionDeclFile.ts")]
#[case("privacyTypeParametersOfClass.ts")]
#[case("privacyTypeParametersOfClassDeclFile.ts")]
#[case("privacyTypeParametersOfInterface.ts")]
#[case("privacyTypeParametersOfInterfaceDeclFile.ts")]
#[case("privacyVar.ts")]
#[case("privacyVarDeclFile.ts")] // NOT RUNNABLE
#[case("privateAccessInSubclass1.ts")]
#[case("privateFieldAssignabilityFromUnknown.ts")]
#[case("privateInstanceVisibility.ts")]
#[case("privateInterfaceProperties.ts")]
#[case("privateNameJsx.tsx")]
#[case("privateNameWeakMapCollision.ts")]
#[case("privatePropertyInUnion.ts")]
#[case("privatePropertyUsingObjectType.ts")]
#[case("privateVisibility.ts")]
#[case("privateVisibles.ts")]
#[case("promiseAllOnAny01.ts")]
#[case("promiseChaining.ts")]
#[case("promiseChaining1.ts")]
#[case("promiseChaining2.ts")]
#[case("promiseDefinitionTest.ts")]
#[case("promiseEmptyTupleNoException.ts")]
#[case("promiseIdentity.ts")]
#[case("promiseIdentity2.ts")]
#[case("promiseIdentityWithAny.ts")]
#[case("promiseIdentityWithAny2.ts")]
#[case("promiseIdentityWithConstraints.ts")]
#[case("promisePermutations.ts")]
#[case("promisePermutations2.ts")]
#[case("promisePermutations3.ts")]
#[case("promiseTest.ts")]
#[case("promiseType.ts")]
#[case("promiseTypeInference.ts")]
#[case("promiseTypeStrictNull.ts")]
#[case("promiseVoidErrorCallback.ts")]
#[case("promises.ts")]
#[case("promisesWithConstraints.ts")]
#[case("propTypeValidatorInference.ts")] // NOT RUNNABLE
#[case("propagateNonInferrableType.ts")]
#[case("propagationOfPromiseInitialization.ts")]
#[case("properties.ts")]
#[case("propertiesAndIndexers.ts")] // FAILING 4298 on Number stuff
#[case("propertiesAndIndexers2.ts")]
#[case("propertiesAndIndexersForNumericNames.ts")] // FAILING 4300 on Number stuff
#[case("propertyAccess1.ts")]
#[case("propertyAccess2.ts")]
#[case("propertyAccess3.ts")]
#[case("propertyAccess4.ts")]
#[case("propertyAccess5.ts")]
#[case("propertyAccess6.ts")]
#[case("propertyAccess7.ts")]
#[case("propertyAccessExpressionInnerComments.ts")]
#[case("propertyAccessOfReadonlyIndexSignature.ts")]
#[case("propertyAccessOnObjectLiteral.ts")]
#[case("propertyAccessibility1.ts")]
#[case("propertyAccessibility2.ts")]
#[case("propertyAssignment.ts")]
#[case("propertyIdentityWithPrivacyMismatch.ts")] // NOT RUNNABLE
#[case("propertyNamesWithStringLiteral.ts")]
#[case("propertyOrdering.ts")]
#[case("propertyOrdering2.ts")]
#[case("propertyOverridingPrototype.ts")]
#[case("propertyParameterWithQuestionMark.ts")]
#[case("propertySignatures.ts")]
#[case("propertyWrappedInTry.ts")]
#[case("protectedMembers.ts")]
#[case("protoAsIndexInIndexExpression.ts")] // NOT RUNNABLE
#[case("protoAssignment.ts")]
#[case("protoInIndexer.ts")]
#[case("prototypeInstantiatedWithBaseConstraint.ts")]
#[case("prototypeOnConstructorFunctions.ts")]
#[case("prototypes.ts")]
#[case("publicGetterProtectedSetterFromThisParameter.ts")]
#[case("publicMemberImplementedAsPrivateInDerivedClass.ts")]
#[case("qualifiedModuleLocals.ts")]
#[case("qualifiedName_ImportDeclarations-entity-names-referencing-a-var.ts")]
#[case("qualifiedName_entity-name-resolution-does-not-affect-class-heritage.ts")]
#[case("qualify.ts")]
#[case("quickIntersectionCheckCorrectlyCachesErrors.ts")] // NOT RUNNABLE
#[case("quickinfoTypeAtReturnPositionsInaccurate.ts")]
#[case("quotedAccessorName1.ts")]
#[case("quotedAccessorName2.ts")]
#[case("quotedFunctionName1.ts")]
#[case("quotedFunctionName2.ts")]
#[case("quotedModuleNameMustBeAmbient.ts")]
#[case("quotedPropertyName1.ts")]
#[case("quotedPropertyName2.ts")]
#[case("quotedPropertyName3.ts")]
#[case("raiseErrorOnParameterProperty.ts")]
#[case("ramdaToolsNoInfinite.ts")]
#[case("ramdaToolsNoInfinite2.ts")]
#[case("randomSemicolons1.ts")]
#[case("reExportGlobalDeclaration1.ts")] // NOT RUNNABLE
#[case("reExportGlobalDeclaration2.ts")] // NOT RUNNABLE
#[case("reExportGlobalDeclaration3.ts")] // NOT RUNNABLE
#[case("reExportGlobalDeclaration4.ts")] // NOT RUNNABLE
#[case("reExportUndefined1.ts")] // NOT RUNNABLE
#[case("reExportUndefined2.ts")] // NOT RUNNABLE
#[case("reachabilityCheckWithEmptyDefault.ts")]
#[case("reachabilityChecks1.ts")] // NOT RUNNABLE on .lib directive
#[case("reachabilityChecks2.ts")]
#[case("reachabilityChecks3.ts")]
#[case("reachabilityChecks4.ts")]
#[case("reachabilityChecks5.ts")]
#[case("reachabilityChecks6.ts")]
#[case("reachabilityChecks7.ts")]
#[case("reachabilityChecks8.ts")]
#[case("reactDefaultPropsInferenceSuccess.tsx")] // NOT RUNNABLE on .lib directive
#[case("reactHOCSpreadprops.tsx")] // NOT RUNNABLE on .lib directive
#[case("reactImportDropped.ts")] // NOT RUNNABLE
#[case("reactImportUnusedInNewJSXEmit.tsx")] // NOT RUNNABLE
#[case("reactJsxReactResolvedNodeNext.tsx")] // NOT RUNNABLE
#[case("reactJsxReactResolvedNodeNextEsm.tsx")] // NOT RUNNABLE
#[case("reactNamespaceImportPresevation.tsx")] // NOT RUNNABLE
#[case("reactNamespaceInvalidInput.tsx")]
#[case("reactNamespaceJSXEmit.tsx")]
#[case("reactNamespaceMissingDeclaration.tsx")]
#[case("reactReadonlyHOCAssignabilityReal.tsx")] // NOT RUNNABLE on .lib directive
#[case("reactReduxLikeDeferredInferenceAllowsAssignment.ts")] // FAILING 4375
#[case("reactSFCAndFunctionResolvable.tsx")] // NOT RUNNABLE on .lib directive
#[case("reactTagNameComponentWithPropsNoOOM.tsx")] // NOT RUNNABLE on .lib directive
#[case("reactTagNameComponentWithPropsNoOOM2.tsx")] // NOT RUNNABLE on .lib directive
#[case("reactTransitiveImportHasValidDeclaration.ts")] // NOT RUNNABLE
#[case("readonlyAssignmentInSubclassOfClassExpression.ts")]
#[case("readonlyFloat32ArrayAssignableWithFloat32Array.ts")]
#[case("readonlyInDeclarationFile.ts")]
#[case("readonlyInNonPropertyParameters.ts")]
#[case("readonlyMembers.ts")]
#[case("readonlyTupleAndArrayElaboration.ts")]
#[case("reassignStaticProp.ts")]
#[case("reboundBaseClassSymbol.ts")]
#[case("reboundIdentifierOnImportAlias.ts")]
#[case("rectype.ts")]
#[case("recur1.ts")]
#[case("recursiveArrayNotCircular.ts")]
#[case("recursiveBaseCheck.ts")]
#[case("recursiveBaseCheck2.ts")]
#[case("recursiveBaseCheck3.ts")]
#[case("recursiveBaseCheck4.ts")]
#[case("recursiveBaseCheck5.ts")]
#[case("recursiveBaseCheck6.ts")]
#[case("recursiveBaseConstructorCreation1.ts")]
#[case("recursiveBaseConstructorCreation2.ts")]
#[case("recursiveBaseConstructorCreation3.ts")]
#[case("recursiveClassBaseType.ts")]
#[case("recursiveClassInstantiationsWithDefaultConstructors.ts")]
#[case("recursiveClassReferenceTest.ts")]
#[case("recursiveCloduleReference.ts")]
#[case("recursiveComplicatedClasses.ts")]
#[case("recursiveConditionalCrash1.ts")]
#[case("recursiveConditionalCrash2.ts")]
#[case("recursiveConditionalCrash3.ts")]
#[case("recursiveConditionalEvaluationNonInfinite.ts")]
#[case("recursiveConditionalTypes.ts")]
#[case("recursiveExcessPropertyChecks.ts")]
#[case("recursiveExportAssignmentAndFindAliasedType1.ts")] // NOT RUNNABLE
#[case("recursiveExportAssignmentAndFindAliasedType2.ts")] // NOT RUNNABLE
#[case("recursiveExportAssignmentAndFindAliasedType3.ts")] // NOT RUNNABLE
#[case("recursiveExportAssignmentAndFindAliasedType4.ts")] // NOT RUNNABLE
#[case("recursiveExportAssignmentAndFindAliasedType5.ts")] // NOT RUNNABLE
#[case("recursiveExportAssignmentAndFindAliasedType6.ts")] // NOT RUNNABLE
#[case("recursiveExportAssignmentAndFindAliasedType7.ts")] // NOT RUNNABLE
#[case("recursiveFieldSetting.ts")]
#[case("recursiveFunctionTypes.ts")]
#[case("recursiveFunctionTypes1.ts")]
#[case("recursiveGenericMethodCall.ts")]
#[case("recursiveGenericSignatureInstantiation.ts")]
#[case("recursiveGenericSignatureInstantiation2.ts")]
#[case("recursiveGenericTypeHierarchy.ts")]
#[case("recursiveGenericUnionType1.ts")]
#[case("recursiveGenericUnionType2.ts")]
#[case("recursiveGetterAccess.ts")]
#[case("recursiveIdenticalAssignment.ts")]
#[case("recursiveIdenticalOverloadResolution.ts")]
#[case("recursiveInference1.ts")]
#[case("recursiveInferenceBug.ts")]
#[case("recursiveInheritance.ts")]
#[case("recursiveInheritance2.ts")]
#[case("recursiveInheritance3.ts")]
#[case("recursiveInheritanceGeneric.ts")]
#[case("recursiveLetConst.ts")]
#[case("recursiveMods.ts")]
#[case("recursiveNamedLambdaCall.ts")]
#[case("recursiveObjectLiteral.ts")]
#[case("recursiveProperties.ts")]
#[case("recursiveResolveDeclaredMembers.ts")] // NOT RUNNABLE
#[case("recursiveResolveTypeMembers.ts")]
#[case("recursiveReturns.ts")]
#[case("recursiveReverseMappedType.ts")]
#[case("recursiveSpecializationOfExtendedTypeWithError.ts")]
#[case("recursiveSpecializationOfSignatures.ts")]
#[case("recursiveTupleTypeInference.ts")]
#[case("recursiveTupleTypes1.ts")]
#[case("recursiveTupleTypes2.ts")]
#[case("recursiveTypeComparison.ts")]
#[case("recursiveTypeComparison2.ts")]
#[case("recursiveTypeIdentity.ts")]
#[case("recursiveTypeParameterConstraintReferenceLacksTypeArgs.ts")]
#[case("recursiveTypeParameterReferenceError1.ts")]
#[case("recursiveTypeParameterReferenceError2.ts")]
#[case("recursiveTypeRelations.ts")]
#[case("recursiveTypes1.ts")]
#[case("recursiveUnionTypeInference.ts")]
#[case("recursivelyExpandingUnionNoStackoverflow.ts")]
#[case("recursivelySpecializedConstructorDeclaration.ts")]
#[case("redeclarationOfVarWithGenericType.ts")]
#[case("redeclareParameterInCatchBlock.ts")]
#[case("redefineArray.ts")]
#[case("reexportDefaultIsCallable.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault1.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault2.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault3.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault4.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault5.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault6.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault7.ts")] // NOT RUNNABLE
#[case("reexportMissingDefault8.ts")] // NOT RUNNABLE
#[case("reexportNameAliasedAndHoisted.ts")] // NOT RUNNABLE
#[case("reexportWrittenCorrectlyInDeclaration.ts")] // NOT RUNNABLE
#[case("reexportedMissingAlias.ts")] // NOT RUNNABLE
#[case("referenceTypesPreferedToPathIfPossible.ts")] // NOT RUNNABLE
#[case("regExpWithSlashInCharClass.ts")]
#[case("regexMatchAll-esnext.ts")]
#[case("regexMatchAll.ts")]
#[case("relationalOperatorComparable.ts")]
#[case("relativeNamesInClassicResolution.ts")] // NOT RUNNABLE
#[case("reorderProperties.ts")]
#[case("requireAsFunctionInExternalModule.ts")] // NOT RUNNABLE
#[case("requireEmitSemicolon.ts")] // NOT RUNNABLE
#[case("requireOfAnEmptyFile1.ts")] // NOT RUNNABLE
#[case("requireOfJsonFile.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileInJsFile.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileNonRelative.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileNonRelativeWithoutExtension.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileNonRelativeWithoutExtensionResolvesToTs.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileTypes.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithAlwaysStrictWithoutErrors.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithAmd.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithComputedPropertyName.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithDeclaration.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithEmptyObject.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithEmptyObjectWithErrors.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithErrors.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleEmitNone.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleEmitUndefined.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitAmd.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitAmdOutFile.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitEs2015.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitEsNext.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitNone.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitSystem.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitUmd.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithModuleNodeResolutionEmitUndefined.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithNoContent.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithSourceMap.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithTraillingComma.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutAllowJs.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutEsModuleInterop.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutExtension.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutExtensionResolvesToTs.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutOutDir.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutResolveJsonModule.ts")] // NOT RUNNABLE
#[case("requireOfJsonFileWithoutResolveJsonModuleAndPathMapping.ts")] // NOT RUNNABLE
#[case("requireOfJsonFile_PathMapping.ts")] // NOT RUNNABLE
#[case("requiredInitializedParameter1.ts")]
#[case("requiredInitializedParameter2.ts")]
#[case("requiredInitializedParameter3.ts")]
#[case("requiredInitializedParameter4.ts")]
#[case("requiredMappedTypeModifierTrumpsVariance.ts")]
#[case("reservedNameOnInterfaceImport.ts")]
#[case("reservedNameOnModuleImport.ts")]
#[case("reservedNameOnModuleImportWithInterface.ts")]
#[case("reservedWords.ts")]
#[case("reservedWords2.ts")]
#[case("reservedWords3.ts")]
#[case("resolveInterfaceNameWithSameLetDeclarationName1.ts")]
#[case("resolveInterfaceNameWithSameLetDeclarationName2.ts")]
#[case("resolveModuleNameWithSameLetDeclarationName1.ts")]
#[case("resolveModuleNameWithSameLetDeclarationName2.ts")]
#[case("resolveNameWithNamspace.ts")] // NOT RUNNABLE
#[case("resolveTypeAliasWithSameLetDeclarationName1.ts")]
#[case("resolvingClassDeclarationWhenInBaseTypeResolution.ts")]
#[case("restArgAssignmentCompat.ts")]
#[case("restArgMissingName.ts")]
#[case("restElementWithNumberPropertyName.ts")]
#[case("restIntersection.ts")]
#[case("restInvalidArgumentType.ts")]
#[case("restParamAsOptional.ts")]
#[case("restParamModifier.ts")]
#[case("restParamModifier2.ts")]
#[case("restParameterAssignmentCompatibility.ts")]
#[case("restParameterNoTypeAnnotation.ts")]
#[case("restParameterNotLast.ts")]
#[case("restParameterTypeInstantiation.ts")]
#[case("restParameterWithBindingPattern1.ts")]
#[case("restParameterWithBindingPattern2.ts")]
#[case("restParameterWithBindingPattern3.ts")]
#[case("restParameters.ts")]
#[case("restParamsWithNonRestParams.ts")]
#[case("restTypeRetainsMappyness.ts")]
#[case("restUnion.ts")]
#[case("restUnion2.ts")]
#[case("restUnion3.ts")]
#[case("returnInConstructor1.ts")]
#[case("returnInfiniteIntersection.ts")]
#[case("returnStatement1.ts")]
#[case("returnTypeInferenceNotTooBroad.ts")]
#[case("returnTypeParameter.ts")]
#[case("returnTypeParameterWithModules.ts")]
#[case("returnTypePredicateIsInstantiateInContextOfTarget.tsx")] // NOT RUNNABLE on .lib directive
#[case("returnTypeTypeArguments.ts")]
#[case("returnValueInSetter.ts")]
#[case("reuseInnerModuleMember.ts")] // NOT RUNNABLE
#[case("reverseInferenceInContextualInstantiation.ts")]
#[case("reverseMappedContravariantInference.ts")]
#[case("reverseMappedPartiallyInferableTypes.ts")]
#[case("reverseMappedTypeAssignableToIndex.ts")]
#[case("reverseMappedTypeContextualTypeNotCircular.ts")]
#[case("reverseMappedTypeDeepDeclarationEmit.ts")]
#[case("reversedRecusiveTypeInstantiation.ts")]
#[case("scopeCheckClassProperty.ts")]
#[case("scopeCheckExtendedClassInsidePublicMethod2.ts")]
#[case("scopeCheckExtendedClassInsideStaticMethod1.ts")]
#[case("scopeCheckInsidePublicMethod1.ts")]
#[case("scopeCheckInsideStaticMethod1.ts")]
#[case("scopeCheckStaticInitializer.ts")]
#[case("scopeTests.ts")]
#[case("scopingInCatchBlocks.ts")]
#[case("selfInCallback.ts")]
#[case("selfInLambdas.ts")]
#[case("selfRef.ts")]
#[case("selfReference.ts")]
#[case("selfReferencesInFunctionParameters.ts")]
#[case("selfReferencingFile.ts")]
#[case("selfReferencingFile2.ts")]
#[case("selfReferencingFile3.ts")] // NOT RUNNABLE on .lib directive
#[case("selfReferencingSpreadInLoop.ts")]
#[case("selfReferencingTypeReferenceInference.ts")]
#[case("selfReferentialDefaultNoStackOverflow.ts")] // NOT RUNNABLE
#[case("semicolonsInModuleDeclarations.ts")]
#[case("separate1-1.ts")]
#[case("separate1-2.ts")]
#[case("setterBeforeGetter.ts")]
#[case("setterWithReturn.ts")]
#[case("shadowPrivateMembers.ts")]
#[case("shadowedReservedCompilerDeclarationsWithNoEmit.ts")]
#[case("shadowingViaLocalValue.ts")]
#[case("shadowingViaLocalValueOrBindingElement.ts")]
#[case("shebang.ts")]
#[case("shebangBeforeReferences.ts")]
#[case("shebangError.ts")]
#[case("shorthand-property-es5-es6.ts")] // NOT RUNNABLE
#[case("shorthand-property-es6-amd.ts")] // NOT RUNNABLE
#[case("shorthand-property-es6-es6.ts")] // NOT RUNNABLE
#[case("shorthandOfExportedEntity01_targetES2015_CommonJS.ts")]
#[case("shorthandOfExportedEntity02_targetES5_CommonJS.ts")]
#[case("shorthandPropertyAssignmentInES6Module.ts")] // NOT RUNNABLE
#[case("shorthandPropertyAssignmentsInDestructuring.ts")]
#[case("shorthandPropertyAssignmentsInDestructuring_ES6.ts")]
#[case("shorthandPropertyUndefined.ts")]
#[case("shouldNotPrintNullEscapesIntoOctalLiterals.ts")]
#[case("sigantureIsSubTypeIfTheyAreIdentical.ts")]
#[case("signatureInstantiationWithRecursiveConstraints.ts")]
#[case("signaturesUseJSDocForOptionalParameters.ts")] // NOT RUNNABLE
#[case("silentNeverPropagation.ts")]
#[case("simpleArrowFunctionParameterReferencedInObjectLiteral1.ts")]
#[case("simplifyingConditionalWithInteriorConditionalIsRelated.ts")]
#[case("singleLineCommentInConciseArrowFunctionES3.ts")]
#[case("slashBeforeVariableDeclaration1.ts")]
#[case("sliceResultCast.ts")]
#[case("slightlyIndirectedDeepObjectLiteralElaborations.ts")]
#[case("sourceMap-Comment1.ts")]
#[case("sourceMap-Comments.ts")]
#[case("sourceMap-Comments2.ts")]
#[case("sourceMap-EmptyFile1.ts")]
#[case("sourceMap-FileWithComments.ts")]
#[case("sourceMap-InterfacePrecedingVariableDeclaration1.ts")]
#[case("sourceMap-LineBreaks.ts")]
#[case("sourceMap-NewLine1.ts")]
#[case("sourceMap-SemiColon1.ts")]
#[case("sourceMap-SingleSpace1.ts")]
#[case("sourceMap-SkippedNode.ts")]
#[case("sourceMap-StringLiteralWithNewLine.ts")]
#[case("sourceMapForFunctionInInternalModuleWithCommentPrecedingStatement01.ts")]
#[case("sourceMapForFunctionWithCommentPrecedingStatement01.ts")]
#[case("sourceMapPercentEncoded.ts")] // NOT RUNNABLE
#[case("sourceMapSample.ts")]
#[case("sourceMapValidationClass.ts")]
#[case("sourceMapValidationClassWithDefaultConstructor.ts")]
#[case("sourceMapValidationClassWithDefaultConstructorAndCapturedThisStatement.ts")]
#[case("sourceMapValidationClassWithDefaultConstructorAndExtendsClause.ts")]
#[case("sourceMapValidationClasses.ts")]
#[case("sourceMapValidationDebugger.ts")]
#[case("sourceMapValidationDecorators.ts")]
#[case("sourceMapValidationDestructuringForArrayBindingPattern.ts")]
#[case("sourceMapValidationDestructuringForArrayBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringForArrayBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringForArrayBindingPatternDefaultValues2.ts")]
#[case("sourceMapValidationDestructuringForObjectBindingPattern.ts")]
#[case("sourceMapValidationDestructuringForObjectBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringForObjectBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringForObjectBindingPatternDefaultValues2.ts")]
#[case("sourceMapValidationDestructuringForOfArrayBindingPattern.ts")]
#[case("sourceMapValidationDestructuringForOfArrayBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringForOfArrayBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringForOfArrayBindingPatternDefaultValues2.ts")]
#[case("sourceMapValidationDestructuringForOfObjectBindingPattern.ts")]
#[case("sourceMapValidationDestructuringForOfObjectBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringForOfObjectBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringForOfObjectBindingPatternDefaultValues2.ts")]
#[case("sourceMapValidationDestructuringParameterNestedObjectBindingPattern.ts")]
#[case("sourceMapValidationDestructuringParameterNestedObjectBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringParameterObjectBindingPattern.ts")]
#[case("sourceMapValidationDestructuringParameterObjectBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringParametertArrayBindingPattern.ts")]
#[case("sourceMapValidationDestructuringParametertArrayBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringParametertArrayBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringParametertArrayBindingPatternDefaultValues2.ts")]
#[case("sourceMapValidationDestructuringVariableStatement.ts")]
#[case("sourceMapValidationDestructuringVariableStatement1.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern3.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern4.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern5.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern6.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPattern7.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPatternDefaultValues.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPatternDefaultValues2.ts")]
#[case("sourceMapValidationDestructuringVariableStatementArrayBindingPatternDefaultValues3.ts")]
#[case("sourceMapValidationDestructuringVariableStatementDefaultValues.ts")]
#[case("sourceMapValidationDestructuringVariableStatementNestedObjectBindingPattern.ts")]
#[case("sourceMapValidationDestructuringVariableStatementNestedObjectBindingPatternWithDefaultValues.ts")]
#[case("sourceMapValidationDestructuringVariableStatementObjectBindingPattern1.ts")]
#[case("sourceMapValidationDestructuringVariableStatementObjectBindingPattern2.ts")]
#[case("sourceMapValidationDestructuringVariableStatementObjectBindingPattern3.ts")]
#[case("sourceMapValidationDestructuringVariableStatementObjectBindingPattern4.ts")]
#[case("sourceMapValidationDo.ts")]
#[case("sourceMapValidationEnums.ts")]
#[case("sourceMapValidationExportAssignment.ts")]
#[case("sourceMapValidationExportAssignmentCommonjs.ts")]
#[case("sourceMapValidationFor.ts")]
#[case("sourceMapValidationForIn.ts")]
#[case("sourceMapValidationFunctionExpressions.ts")]
#[case("sourceMapValidationFunctionPropertyAssignment.ts")]
#[case("sourceMapValidationFunctions.ts")]
#[case("sourceMapValidationIfElse.ts")]
#[case("sourceMapValidationImport.ts")]
#[case("sourceMapValidationLabeled.ts")]
#[case("sourceMapValidationLambdaSpanningMultipleLines.ts")]
#[case("sourceMapValidationModule.ts")]
#[case("sourceMapValidationStatements.ts")]
#[case("sourceMapValidationSwitch.ts")]
#[case("sourceMapValidationTryCatchFinally.ts")]
#[case("sourceMapValidationVarInDownLevelGenerator.ts")]
#[case("sourceMapValidationVariables.ts")]
#[case("sourceMapValidationWhile.ts")]
#[case("sourceMapValidationWithComments.ts")]
#[case("sourceMapWithCaseSensitiveFileNames.ts")] // NOT RUNNABLE
#[case("sourceMapWithCaseSensitiveFileNamesAndOutDir.ts")] // NOT RUNNABLE
#[case("sourceMapWithMultipleFilesWithCopyright.ts")] // NOT RUNNABLE
#[case("sourceMapWithMultipleFilesWithFileEndingWithInterface.ts")] // NOT RUNNABLE
#[case("sourceMapWithNonCaseSensitiveFileNames.ts")] // NOT RUNNABLE
#[case("sourceMapWithNonCaseSensitiveFileNamesAndOutDir.ts")] // NOT RUNNABLE
#[case("sourcemapValidationDuplicateNames.ts")]
#[case("spaceBeforeQuestionMarkInPropertyAssignment.ts")]
#[case("specedNoStackBlown.ts")]
#[case("specializationError.ts")]
#[case("specializationOfExportedClass.ts")]
#[case("specializationsShouldNotAffectEachOther.ts")]
#[case("specializeVarArgs1.ts")]
#[case("specializedInheritedConstructors1.ts")]
#[case("specializedLambdaTypeArguments.ts")]
#[case("specializedOverloadWithRestParameters.ts")]
#[case("specializedSignatureAsCallbackParameter1.ts")]
#[case("specializedSignatureInInterface.ts")]
#[case("specializedSignatureOverloadReturnTypeWithIndexers.ts")]
#[case("spellingSuggestionGlobal1.ts")]
#[case("spellingSuggestionGlobal2.ts")]
#[case("spellingSuggestionGlobal3.ts")]
#[case("spellingSuggestionGlobal4.ts")]
#[case("spellingSuggestionJSXAttribute.tsx")] // NOT RUNNABLE on .lib directive
#[case("spellingSuggestionLeadingUnderscores01.ts")]
#[case("spellingSuggestionModule.ts")]
#[case("spliceTuples.ts")]
#[case("spreadBooleanRespectsFreshness.ts")]
#[case("spreadExpressionContextualType.ts")]
#[case("spreadIdenticalTypesRemoved.ts")]
#[case("spreadIntersection.ts")]
#[case("spreadIntersectionJsx.tsx")]
#[case("spreadInvalidArgumentType.ts")]
#[case("spreadObjectWithIndexDoesNotAddUndefinedToLocalIndex.ts")]
#[case("spreadOfObjectLiteralAssignableToIndexSignature.ts")]
#[case("spreadOfParamsFromGeneratorMakesRequiredParams.ts")]
#[case("spreadTypeRemovesReadonly.ts")]
#[case("spyComparisonChecking.ts")]
#[case("stackDepthLimitCastingType.ts")] // NOT RUNNABLE
#[case("standaloneBreak.ts")]
#[case("staticAndMemberFunctions.ts")]
#[case("staticAnonymousTypeNotReferencingTypeParameter.ts")]
#[case("staticAsIdentifier.ts")]
#[case("staticClassMemberError.ts")]
#[case("staticClassProps.ts")]
#[case("staticFieldWithInterfaceContext.ts")]
#[case("staticGetter1.ts")]
#[case("staticGetter2.ts")]
#[case("staticGetterAndSetter.ts")]
#[case("staticIndexSignatureAndNormalIndexSignature.ts")]
#[case("staticInheritance.ts")]
#[case("staticInstanceResolution.ts")]
#[case("staticInstanceResolution2.ts")]
#[case("staticInstanceResolution3.ts")] // NOT RUNNABLE
#[case("staticInstanceResolution4.ts")]
#[case("staticInstanceResolution5.ts")] // NOT RUNNABLE
#[case("staticInterfaceAssignmentCompat.ts")]
#[case("staticMemberAccessOffDerivedType1.ts")]
#[case("staticMemberExportAccess.ts")]
#[case("staticMemberOfClassAndPublicMemberOfAnotherClassAssignment.ts")]
#[case("staticMemberWithStringAndNumberNames.ts")]
#[case("staticMethodReferencingTypeArgument1.ts")]
#[case("staticMethodWithTypeParameterExtendsClauseDeclFile.ts")]
#[case("staticMethodsReferencingClassTypeParameters.ts")]
#[case("staticMismatchBecauseOfPrototype.ts")]
#[case("staticModifierAlreadySeen.ts")]
#[case("staticMustPrecedePublic.ts")]
#[case("staticOffOfInstance1.ts")]
#[case("staticOffOfInstance2.ts")]
#[case("staticPropSuper.ts")]
#[case("staticPrototypeProperty.ts")]
#[case("staticPrototypePropertyOnClass.ts")]
#[case("staticVisibility.ts")]
#[case("staticVisibility2.ts")]
#[case("statics.ts")]
#[case("staticsInAFunction.ts")]
#[case("staticsInConstructorBodies.ts")]
#[case("staticsNotInScopeInClodule.ts")]
#[case("stradac.ts")]
#[case("strictBooleanMemberAssignability.ts")]
#[case("strictFunctionTypes1.ts")]
#[case("strictFunctionTypesErrors.ts")]
#[case("strictModeEnumMemberNameReserved.ts")]
#[case("strictModeInConstructor.ts")]
#[case("strictModeReservedWord.ts")]
#[case("strictModeReservedWord2.ts")]
#[case("strictModeReservedWordInClassDeclaration.ts")]
#[case("strictModeReservedWordInDestructuring.ts")]
#[case("strictModeReservedWordInImportEqualDeclaration.ts")]
#[case("strictModeReservedWordInModuleDeclaration.ts")]
#[case("strictModeUseContextualKeyword.ts")]
#[case("strictModeWordInExportDeclaration.ts")]
#[case("strictModeWordInImportDeclaration.ts")]
#[case("strictNullEmptyDestructuring.ts")]
#[case("strictNullLogicalAndOr.ts")]
#[case("strictNullNotNullIndexTypeNoLib.ts")]
#[case("strictNullNotNullIndexTypeShouldWork.ts")]
#[case("strictOptionalProperties1.ts")]
#[case("strictOptionalProperties2.ts")]
#[case("strictTypeofUnionNarrowing.ts")]
#[case("stringHasStringValuedNumericIndexer.ts")]
#[case("stringIncludes.ts")]
#[case("stringIndexerAndConstructor.ts")]
#[case("stringIndexerAndConstructor1.ts")]
#[case("stringIndexerAssignments1.ts")]
#[case("stringIndexerAssignments2.ts")]
#[case("stringLiteralObjectLiteralDeclaration1.ts")]
#[case("stringLiteralPropertyNameWithLineContinuation1.ts")]
#[case("stringLiteralsErrors.ts")]
#[case("stringMatchAll.ts")]
#[case("stringPropCodeGen.ts")]
#[case("stringRawType.ts")]
#[case("stringTrim.ts")]
#[case("stripInternal1.ts")]
#[case("structural1.ts")]
#[case("structuralTypeInDeclareFileForModule.ts")]
#[case("styleOptions.ts")]
#[case("styledComponentsInstantiaionLimitNotReached.ts")] // NOT RUNNABLE on .lib directive
#[case("subSubClassCanAccessProtectedConstructor.ts")]
#[case("subclassThisTypeAssignable01.ts")] // NOT RUNNABLE
#[case("subclassThisTypeAssignable02.ts")] // NOT RUNNABLE
#[case("subclassWithPolymorphicThisIsAssignable.ts")]
#[case("substituteReturnTypeSatisfiesConstraint.ts")]
#[case("substitutionTypeNoMergeOfAssignableType.ts")]
#[case("substitutionTypesCompareCorrectlyInRestrictiveInstances.ts")]
#[case("substitutionTypesInIndexedAccessTypes.ts")]
#[case("subtypeRelationForNever.ts")]
#[case("subtypingTransitivity.ts")]
#[case("super.ts")]
#[case("super1.ts")]
#[case("super2.ts")]
#[case("superAccess.ts")]
#[case("superAccess2.ts")]
#[case("superAccessCastedCall.ts")]
#[case("superAccessInFatArrow1.ts")]
#[case("superCallArgsMustMatch.ts")]
#[case("superCallAssignResult.ts")]
#[case("superCallFromClassThatDerivesFromGenericType1.ts")]
#[case("superCallFromClassThatDerivesFromGenericType2.ts")]
#[case("superCallFromClassThatDerivesFromGenericTypeButWithIncorrectNumberOfTypeArguments1.ts")]
#[case("superCallFromClassThatDerivesFromGenericTypeButWithNoTypeArguments1.ts")]
#[case("superCallFromClassThatDerivesNonGenericTypeButWithTypeArguments1.ts")]
#[case("superCallFromClassThatHasNoBaseType1.ts")]
#[case("superCallFromFunction1.ts")]
#[case("superCallInNonStaticMethod.ts")]
#[case("superCallInStaticMethod.ts")]
#[case("superCallInsideClassDeclaration.ts")]
#[case("superCallInsideClassExpression.ts")]
#[case("superCallInsideObjectLiteralExpression.ts")]
#[case("superCallOutsideConstructor.ts")]
#[case("superCallWithCommentEmit01.ts")]
#[case("superCallWithMissingBaseClass.ts")]
#[case("superCallsInConstructor.ts")]
#[case("superElementAccess.ts")]
#[case("superErrors.ts")]
#[case("superHasMethodsFromMergedInterface.ts")]
#[case("superInCatchBlock1.ts")]
#[case("superInConstructorParam1.ts")]
#[case("superInLambdas.ts")]
#[case("superInObjectLiterals_ES5.ts")]
#[case("superInObjectLiterals_ES6.ts")]
#[case("superNewCall1.ts")]
#[case("superNoModifiersCrash.ts")] // NOT RUNNABLE
#[case("superPropertyAccess.ts")]
#[case("superPropertyAccess1.ts")]
#[case("superPropertyAccess2.ts")]
#[case("superPropertyAccessInComputedPropertiesOfNestedType_ES5.ts")]
#[case("superPropertyAccessInComputedPropertiesOfNestedType_ES6.ts")]
#[case("superPropertyAccessInSuperCall01.ts")]
#[case("superPropertyAccess_ES5.ts")]
#[case("superPropertyAccess_ES6.ts")]
#[case("superPropertyElementNoUnusedLexicalThisCapture.ts")]
#[case("superWithGenericSpecialization.ts")]
#[case("superWithGenerics.ts")]
#[case("superWithTypeArgument.ts")]
#[case("superWithTypeArgument2.ts")]
#[case("superWithTypeArgument3.ts")]
#[case("super_inside-object-literal-getters-and-setters.ts")]
#[case("switchAssignmentCompat.ts")]
#[case("switchCaseCircularRefeference.ts")]
#[case("switchCaseInternalComments.ts")]
#[case("switchCaseNarrowsMatchingClausesEvenWhenNonMatchingClausesExist.ts")]
#[case("switchCases.ts")]
#[case("switchCasesExpressionTypeMismatch.ts")]
#[case("switchComparableCompatForBrands.ts")]
#[case("switchFallThroughs.ts")]
#[case("switchStatementsWithMultipleDefaults.ts")]
#[case("switchStatementsWithMultipleDefaults1.ts")]
#[case("symbolLinkDeclarationEmitModuleNames.ts")] // NOT RUNNABLE
#[case("symbolLinkDeclarationEmitModuleNamesImportRef.ts")] // NOT RUNNABLE
#[case("symbolLinkDeclarationEmitModuleNamesRootDir.ts")] // NOT RUNNABLE
#[case("symbolMergeValueAndImportedType.ts")] // NOT RUNNABLE
#[case("symbolObserverMismatchingPolyfillsWorkTogether.ts")]
#[case("syntheticDefaultExportsWithDynamicImports.ts")] // NOT RUNNABLE
#[case("systemDefaultExportCommentValidity.ts")]
#[case("systemDefaultImportCallable.ts")] // NOT RUNNABLE
#[case("systemExportAssignment.ts")] // NOT RUNNABLE
#[case("systemExportAssignment2.ts")] // NOT RUNNABLE
#[case("systemExportAssignment3.ts")] // NOT RUNNABLE
#[case("systemJsForInNoException.ts")]
#[case("systemModule1.ts")]
#[case("systemModule10.ts")]
#[case("systemModule10_ES5.ts")]
#[case("systemModule11.ts")] // NOT RUNNABLE
#[case("systemModule12.ts")]
#[case("systemModule13.ts")]
#[case("systemModule14.ts")]
#[case("systemModule15.ts")] // NOT RUNNABLE
#[case("systemModule16.ts")]
#[case("systemModule17.ts")] // NOT RUNNABLE
#[case("systemModule2.ts")]
#[case("systemModule3.ts")] // NOT RUNNABLE
#[case("systemModule4.ts")]
#[case("systemModule5.ts")]
#[case("systemModule6.ts")]
#[case("systemModule7.ts")]
#[case("systemModule8.ts")]
#[case("systemModule9.ts")]
#[case("systemModuleAmbientDeclarations.ts")] // NOT RUNNABLE
#[case("systemModuleConstEnums.ts")]
#[case("systemModuleConstEnumsSeparateCompilation.ts")]
#[case("systemModuleDeclarationMerging.ts")]
#[case("systemModuleExportDefault.ts")] // NOT RUNNABLE
#[case("systemModuleNonTopLevelModuleMembers.ts")]
#[case("systemModuleTargetES6.ts")]
#[case("systemModuleTrailingComments.ts")]
#[case("systemModuleWithSuperClass.ts")] // NOT RUNNABLE
#[case("systemNamespaceAliasEmit.ts")]
#[case("systemObjectShorthandRename.ts")] // NOT RUNNABLE
#[case("taggedPrimitiveNarrowing.ts")]
#[case("taggedTemplateStringWithSymbolExpression01.ts")]
#[case("taggedTemplateStringsHexadecimalEscapes.ts")]
#[case("taggedTemplateStringsHexadecimalEscapesES6.ts")]
#[case("taggedTemplateStringsWithCurriedFunction.ts")]
#[case("taggedTemplateStringsWithMultilineTemplate.ts")]
#[case("taggedTemplateStringsWithMultilineTemplateES6.ts")]
#[case("taggedTemplateStringsWithUnicodeEscapes.ts")]
#[case("taggedTemplateStringsWithUnicodeEscapesES6.ts")]
#[case("taggedTemplateStringsWithWhitespaceEscapes.ts")]
#[case("taggedTemplateStringsWithWhitespaceEscapesES6.ts")]
#[case("taggedTemplateWithoutDeclaredHelper.ts")] // NOT RUNNABLE
#[case("taggedTemplatesInDifferentScopes.ts")]
#[case("taggedTemplatesInModuleAndGlobal.ts")] // NOT RUNNABLE
#[case("taggedTemplatesWithIncompleteNoSubstitutionTemplate1.ts")]
#[case("taggedTemplatesWithIncompleteNoSubstitutionTemplate2.ts")]
#[case("taggedTemplatesWithIncompleteTemplateExpressions1.ts")]
#[case("taggedTemplatesWithIncompleteTemplateExpressions2.ts")]
#[case("taggedTemplatesWithIncompleteTemplateExpressions3.ts")]
#[case("taggedTemplatesWithIncompleteTemplateExpressions4.ts")]
#[case("taggedTemplatesWithIncompleteTemplateExpressions5.ts")]
#[case("taggedTemplatesWithIncompleteTemplateExpressions6.ts")]
#[case("tailRecursiveConditionalTypes.ts")]
#[case("targetEs6DecoratorMetadataImportNotElided.ts")] // NOT RUNNABLE
#[case("targetTypeArgs.ts")]
#[case("targetTypeBaseCalls.ts")]
#[case("targetTypeCalls.ts")]
#[case("targetTypeCastTest.ts")]
#[case("targetTypeObjectLiteral.ts")]
#[case("targetTypeObjectLiteralToAny.ts")]
#[case("targetTypeTest1.ts")]
#[case("targetTypeTest2.ts")]
#[case("targetTypeTest3.ts")]
#[case("targetTypeVoidFunc.ts")]
#[case("targetTypingOnFunctions.ts")]
#[case("templateLiteralsAndDecoratorMetadata.ts")]
#[case("templateLiteralsInTypes.ts")]
#[case("templateLiteralsSourceMap.ts")]
#[case("templateStringsArrayTypeDefinedInES5Mode.ts")]
#[case("templateStringsArrayTypeNotDefinedES5Mode.ts")]
#[case("templateStringsArrayTypeRedefinedInES6Mode.ts")]
#[case("ternaryExpressionSourceMap.ts")]
#[case("testContainerList.ts")]
#[case("testTypings.ts")]
#[case("thisBinding.ts")]
#[case("thisBinding2.ts")]
#[case("thisCapture1.ts")]
#[case("thisConditionalOnMethodReturnOfGenericInstance.ts")]
#[case("thisExpressionInCallExpressionWithTypeArguments.ts")]
#[case("thisExpressionInIndexExpression.ts")]
#[case("thisExpressionOfGenericObject.ts")]
#[case("thisInAccessors.ts")]
#[case("thisInArrowFunctionInStaticInitializer1.ts")]
#[case("thisInClassBodyStaticESNext.ts")]
#[case("thisInConstructorParameter1.ts")]
#[case("thisInConstructorParameter2.ts")]
#[case("thisInGenericStaticMembers.ts")]
#[case("thisInInnerFunctions.ts")]
#[case("thisInLambda.ts")]
#[case("thisInModule.ts")]
#[case("thisInModuleFunction1.ts")]
#[case("thisInOuterClassBody.ts")]
#[case("thisInPropertyBoundDeclarations.ts")]
#[case("thisInStaticMethod1.ts")]
#[case("thisInStatics.ts")]
#[case("thisInSuperCall.ts")]
#[case("thisInSuperCall1.ts")]
#[case("thisInSuperCall2.ts")]
#[case("thisInSuperCall3.ts")]
#[case("thisInTupleTypeParameterConstraints.ts")]
#[case("thisIndexOnExistingReadonlyFieldIsNotNever.ts")]
#[case("thisKeyword.ts")]
#[case("thisReferencedInFunctionInsideArrowFunction1.ts")]
#[case("thisShadowingErrorSpans.ts")]
#[case("thisTypeAsConstraint.ts")]
#[case("thisWhenTypeCheckFails.ts")]
#[case("this_inside-enum-should-not-be-allowed.ts")]
#[case("this_inside-object-literal-getters-and-setters.ts")]
#[case("throwWithoutNewLine1.ts")]
#[case("throwWithoutNewLine2.ts")]
#[case("toStringOnPrimitives.ts")]
#[case("tooFewArgumentsInGenericFunctionTypedArgument.ts")]
#[case("tooManyTypeParameters1.ts")]
#[case("topLevel.ts")]
#[case("topLevelBlockExpando.ts")] // NOT RUNNABLE
#[case("topLevelExports.ts")]
#[case("topLevelLambda.ts")]
#[case("topLevelLambda2.ts")]
#[case("topLevelLambda3.ts")]
#[case("topLevelLambda4.ts")]
#[case("trailingCommaInHeterogenousArrayLiteral1.ts")]
#[case("trailingCommasES3.ts")]
#[case("trailingCommasES5.ts")]
#[case("transformArrowInBlockScopedLoopVarInitializer.ts")]
#[case("transformNestedGeneratorsWithTry.ts")] // NOT RUNNABLE
#[case("transformParenthesizesConditionalSubexpression.ts")]
#[case("transformsElideNullUndefinedType.ts")]
#[case("transitiveTypeArgumentInference1.ts")]
#[case("tripleSlashInCommentNotParsed.ts")]
#[case("tripleSlashReferenceAbsoluteWindowsPath.ts")] // NOT RUNNABLE
#[case("truthinessCallExpressionCoercion.ts")]
#[case("truthinessCallExpressionCoercion1.ts")]
#[case("truthinessCallExpressionCoercion2.ts")]
#[case("truthinessCallExpressionCoercion3.ts")]
#[case("truthinessPromiseCoercion.ts")]
#[case("tryCatchFinally.ts")]
#[case("tryCatchFinallyControlFlow.ts")]
#[case("tryStatementInternalComments.ts")]
#[case("tsconfigMapOptionsAreCaseInsensitive.ts")] // NOT RUNNABLE
#[case("tslibInJs.ts")] // NOT RUNNABLE
#[case("tsxAttributeQuickinfoTypesSameAsObjectLiteral.tsx")]
#[case("tsxAttributesHasInferrableIndex.tsx")]
#[case("tsxDeepAttributeAssignabilityError.tsx")] // NOT RUNNABLE
#[case("tsxDefaultImports.ts")] // NOT RUNNABLE
#[case("tsxDiscriminantPropertyInference.tsx")]
#[case("tsxFragmentChildrenCheck.ts")] // NOT RUNNABLE
#[case("tsxInferenceShouldNotYieldAnyOnUnions.tsx")] // NOT RUNNABLE
#[case("tsxInvokeComponentType.tsx")] // NOT RUNNABLE on .lib directive
#[case("tsxNoTypeAnnotatedSFC.tsx")]
#[case("tsxNotUsingApparentTypeOfSFC.tsx")] // NOT RUNNABLE on .lib directive
#[case("tsxReactPropsInferenceSucceedsOnIntersections.tsx")] // NOT RUNNABLE on .lib directive
#[case("tsxResolveExternalModuleExportsTypes.ts")] // NOT RUNNABLE
#[case("tsxSpreadDoesNotReportExcessProps.tsx")] // NOT RUNNABLE on .lib directive
#[case("tsxStatelessComponentDefaultProps.tsx")] // NOT RUNNABLE on .lib directive
#[case("tsxTypeArgumentPartialDefinitionStillErrors.ts")] // NOT RUNNABLE
#[case("tsxUnionMemberChecksFilterDataProps.tsx")] // NOT RUNNABLE on .lib directive
#[case("tsxUnionSpread.tsx")] // NOT RUNNABLE
#[case("tupleTypeInference.ts")]
#[case("tupleTypeInference2.ts")]
#[case("tupleTypes.ts")]
#[case("twiceNestedKeyofIndexInference.ts")]
#[case("typeAliasDeclarationEmit.ts")]
#[case("typeAliasDeclarationEmit2.ts")]
#[case("typeAliasDeclarationEmit3.ts")]
#[case("typeAliasDeclareKeyword01.d.ts")]
#[case("typeAliasDoesntMakeModuleInstantiated.ts")]
#[case("typeAliasExport.ts")]
#[case("typeAliasFunctionTypeSharedSymbol.ts")]
#[case("typeAnnotationBestCommonTypeInArrayLiteral.ts")]
#[case("typeArgInference.ts")]
#[case("typeArgInference2.ts")]
#[case("typeArgInference2WithError.ts")]
#[case("typeArgInferenceWithNull.ts")]
#[case("typeArgumentConstraintResolution1.ts")]
#[case("typeArgumentDefaultUsesConstraintOnCircularDefault.ts")]
#[case("typeArgumentInferenceApparentType1.ts")]
#[case("typeArgumentInferenceApparentType2.ts")]
#[case("typeArgumentInferenceOrdering.ts")]
#[case("typeArgumentInferenceWithConstraintAsCommonRoot.ts")]
#[case("typeArgumentInferenceWithRecursivelyReferencedTypeAliasToTypeLiteral01.ts")]
#[case("typeArgumentInferenceWithRecursivelyReferencedTypeAliasToTypeLiteral02.ts")]
#[case("typeArgumentsInFunctionExpressions.ts")]
#[case("typeArgumentsOnFunctionsWithNoTypeParameters.ts")]
#[case("typeArgumentsShouldDisallowNonGenericOverloads.ts")]
#[case("typeAssertionToGenericFunctionType.ts")]
#[case("typeCheckObjectCreationExpressionWithUndefinedCallResolutionData.ts")] // NOT RUNNABLE
#[case("typeCheckObjectLiteralMethodBody.ts")]
#[case("typeCheckReturnExpression.ts")]
#[case("typeCheckTypeArgument.ts")]
#[case("typeCheckingInsideFunctionExpressionInArray.ts")]
#[case("typeComparisonCaching.ts")]
#[case("typeConstraintsWithConstructSignatures.ts")]
#[case("typeGuardConstructorClassAndNumber.ts")]
#[case("typeGuardConstructorDerivedClass.ts")]
#[case("typeGuardConstructorNarrowAny.ts")]
#[case("typeGuardConstructorNarrowPrimitivesInUnion.ts")]
#[case("typeGuardConstructorPrimitiveTypes.ts")]
#[case("typeGuardNarrowsIndexedAccessOfKnownProperty.ts")]
#[case("typeGuardOnContainerTypeNoHang.ts")]
#[case("typeIdentityConsidersBrands.ts")]
#[case("typeInfer1.ts")]
#[case("typeInferenceCacheInvalidation.ts")]
#[case("typeInferenceConflictingCandidates.ts")]
#[case("typeInferenceFBoundedTypeParams.ts")]
#[case("typeInferenceFixEarly.ts")]
#[case("typeInferenceLiteralUnion.ts")]
#[case("typeInferenceReturnTypeCallback.ts")]
#[case("typeInferenceTypePredicate.ts")]
#[case("typeInferenceTypePredicate2.ts")]
#[case("typeInferenceWithExcessProperties.ts")]
#[case("typeInferenceWithTypeAnnotation.ts")]
#[case("typeLiteralCallback.ts")]
#[case("typeMatch1.ts")]
#[case("typeMatch2.ts")]
#[case("typeName1.ts")]
#[case("typeOfEnumAndVarRedeclarations.ts")]
#[case("typeOfOnTypeArg.ts")]
#[case("typeOfOperator1.ts")]
#[case("typeOfPrototype.ts")]
#[case("typeOfSuperCall.ts")]
#[case("typeOfThisInStatics.ts")]
#[case("typeParamExtendsOtherTypeParam.ts")]
#[case("typeParameterAndArgumentOfSameName1.ts")]
#[case("typeParameterArgumentEquivalence.ts")]
#[case("typeParameterArgumentEquivalence2.ts")]
#[case("typeParameterArgumentEquivalence3.ts")]
#[case("typeParameterArgumentEquivalence4.ts")]
#[case("typeParameterArgumentEquivalence5.ts")]
#[case("typeParameterAsBaseClass.ts")]
#[case("typeParameterAsElementType.ts")]
#[case("typeParameterAssignmentCompat1.ts")]
#[case("typeParameterAssignmentWithConstraints.ts")]
#[case("typeParameterCompatibilityAccrossDeclarations.ts")]
#[case("typeParameterConstrainedToOuterTypeParameter.ts")]
#[case("typeParameterConstrainedToOuterTypeParameter2.ts")]
#[case("typeParameterConstraintInstantiation.ts")]
#[case("typeParameterConstraints1.ts")]
#[case("typeParameterDiamond1.ts")]
#[case("typeParameterDiamond2.ts")]
#[case("typeParameterDiamond3.ts")]
#[case("typeParameterDiamond4.ts")]
#[case("typeParameterDoesntBlockParameterLookup.ts")]
#[case("typeParameterEquality.ts")]
#[case("typeParameterExplicitlyExtendsAny.ts")]
#[case("typeParameterExtendingUnion1.ts")]
#[case("typeParameterExtendingUnion2.ts")]
#[case("typeParameterExtendsPrimitive.ts")]
#[case("typeParameterFixingWithConstraints.ts")]
#[case("typeParameterFixingWithContextSensitiveArguments.ts")]
#[case("typeParameterFixingWithContextSensitiveArguments2.ts")]
#[case("typeParameterFixingWithContextSensitiveArguments3.ts")]
#[case("typeParameterFixingWithContextSensitiveArguments4.ts")]
#[case("typeParameterFixingWithContextSensitiveArguments5.ts")]
#[case("typeParameterHasSelfAsConstraint.ts")]
#[case("typeParameterInConstraint1.ts")]
#[case("typeParameterLeak.ts")]
#[case("typeParameterListWithTrailingComma1.ts")]
#[case("typeParameterOrderReversal.ts")]
#[case("typeParameterWithInvalidConstraintType.ts")]
#[case("typeParametersAndParametersInComputedNames.ts")]
#[case("typeParametersInStaticAccessors.ts")]
#[case("typeParametersInStaticMethods.ts")]
#[case("typeParametersInStaticProperties.ts")]
#[case("typeParametersShouldNotBeEqual.ts")]
#[case("typeParametersShouldNotBeEqual2.ts")]
#[case("typeParametersShouldNotBeEqual3.ts")]
#[case("typePartameterConstraintInstantiatedWithDefaultWhenCheckingDefault.ts")]
#[case("typePredicateInLoop.ts")]
#[case("typePredicateStructuralMatch.ts")]
#[case("typePredicateWithThisParameter.ts")]
#[case("typePredicatesInUnion.ts")]
#[case("typePredicatesInUnion2.ts")]
#[case("typePredicatesInUnion_noMatch.ts")]
#[case("typeReferenceDirectives1.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives10.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives11.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives12.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives13.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives2.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives3.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives4.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives5.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives6.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives7.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives8.ts")] // NOT RUNNABLE
#[case("typeReferenceDirectives9.ts")] // NOT RUNNABLE
#[case("typeResolution.ts")]
#[case("typeRootsFromMultipleNodeModulesDirectories.ts")] // NOT RUNNABLE
#[case("typeRootsFromNodeModulesInParentDirectory.ts")] // NOT RUNNABLE
#[case("typeUsedAsTypeLiteralIndex.ts")]
#[case("typeUsedAsValueError.ts")]
#[case("typeUsedAsValueError2.ts")] // NOT RUNNABLE
#[case("typeVal.ts")]
#[case("typeValueConflict1.ts")]
#[case("typeValueConflict2.ts")]
#[case("typeVariableTypeGuards.ts")]
#[case("typecheckCommaExpression.ts")]
#[case("typecheckIfCondition.ts")]
#[case("typedArrays-es5.ts")]
#[case("typedArrays-es6.ts")]
#[case("typedArrays.ts")]
#[case("typedArraysCrossAssignability01.ts")]
#[case("typedArraysSubarray.ts")]
#[case("typedGenericPrototypeMember.ts")]
#[case("typeofAmbientExternalModules.ts")] // NOT RUNNABLE
#[case("typeofClass.ts")]
#[case("typeofEnum.ts")]
#[case("typeofExternalModules.ts")] // NOT RUNNABLE
#[case("typeofInObjectLiteralType.ts")]
#[case("typeofInterface.ts")]
#[case("typeofInternalModules.ts")]
#[case("typeofProperty.ts")]
#[case("typeofSimple.ts")]
#[case("typeofStrictNull.ts")]
#[case("typeofStripsFreshness.ts")]
#[case("typeofUndefined.ts")]
#[case("typeofUnknownSymbol.ts")]
#[case("typeofUsedBeforeBlockScoped.ts")]
#[case("umdDependencyComment2.ts")]
#[case("umdDependencyCommentName1.ts")]
#[case("umdDependencyCommentName2.ts")]
#[case("umdGlobalAugmentationNoCrash.ts")] // NOT RUNNABLE
#[case("umdGlobalConflict.ts")] // NOT RUNNABLE
#[case("umdNamedAmdMode.ts")] // NOT RUNNABLE
#[case("umdNamespaceMergedWithGlobalAugmentationIsNotCircular.ts")] // NOT RUNNABLE
#[case("unaryOperators1.ts")]
#[case("unaryOperatorsInStrictMode.ts")]
#[case("unaryPlus.ts")]
#[case("uncaughtCompilerError1.ts")]
#[case("uncaughtCompilerError2.ts")]
#[case("unclosedExportClause01.ts")] // NOT RUNNABLE
#[case("unclosedExportClause02.ts")] // NOT RUNNABLE
#[case("undeclaredBase.ts")]
#[case("undeclaredMethod.ts")]
#[case("undeclaredModuleError.ts")]
#[case("undeclaredVarEmit.ts")]
#[case("undefinedArgumentInference.ts")]
#[case("undefinedAssignableToGenericMappedIntersection.ts")]
#[case("undefinedInferentialTyping.ts")]
#[case("undefinedSymbolReferencedInArrayLiteral1.ts")]
#[case("undefinedTypeArgument1.ts")]
#[case("undefinedTypeArgument2.ts")]
#[case("undefinedTypeAssignment1.ts")]
#[case("undefinedTypeAssignment2.ts")]
#[case("undefinedTypeAssignment3.ts")]
#[case("undefinedTypeAssignment4.ts")]
#[case("underscoreEscapedNameInEnum.ts")]
#[case("underscoreMapFirst.ts")]
#[case("underscoreTest1.ts")] // NOT RUNNABLE
#[case("underscoreThisInDerivedClass01.ts")]
#[case("underscoreThisInDerivedClass02.ts")]
#[case("unexpectedStatementBlockTerminator.ts")]
#[case("unexportedInstanceClassVariables.ts")]
#[case("unicodeEscapesInJSDoc.ts")] // NOT RUNNABLE
#[case("unicodeIdentifierName2.ts")]
#[case("unicodeIdentifierNames.ts")]
#[case("unicodeStringLiteral.ts")]
#[case("unionErrorMessageOnMatchingDiscriminant.ts")]
#[case("unionExcessPropertyCheckNoApparentPropTypeMismatchErrors.ts")]
#[case("unionExcessPropsWithPartialMember.ts")]
#[case("unionOfClassCalls.ts")]
#[case("unionOfEnumInference.ts")]
#[case("unionOfFunctionAndSignatureIsCallable.ts")]
#[case("unionPropertyExistence.ts")]
#[case("unionReductionMutualSubtypes.ts")]
#[case("unionRelationshipCheckPasses.ts")]
#[case("unionSignaturesWithThisParameter.ts")]
#[case("unionSubtypeReductionErrors.ts")]
#[case("unionTypeErrorMessageTypeRefs01.ts")]
#[case("unionTypeParameterInference.ts")]
#[case("unionTypeWithIndexAndMethodSignature.ts")]
#[case("unionTypeWithIndexAndTuple.ts")]
#[case("unionTypeWithIndexedLiteralType.ts")]
#[case("unionTypeWithLeadingOperator.ts")]
#[case("unionTypeWithRecursiveSubtypeReduction1.ts")]
#[case("unionTypeWithRecursiveSubtypeReduction2.ts")]
#[case("unionTypeWithRecursiveSubtypeReduction3.ts")]
#[case("unionWithIndexSignature.ts")]
#[case("uniqueSymbolAllowsIndexInObjectWithIndexSignature.ts")]
#[case("uniqueSymbolAssignmentOnGlobalAugmentationSuceeds.ts")]
#[case("uniqueSymbolPropertyDeclarationEmit.ts")] // NOT RUNNABLE
#[case("unknownPropertiesAreAssignableToObjectUnion.ts")]
#[case("unknownSymbolInGenericReturnType.ts")]
#[case("unknownSymbolOffContextualType1.ts")]
#[case("unknownSymbols1.ts")]
#[case("unknownSymbols2.ts")]
#[case("unknownTypeArgOnCall.ts")]
#[case("unknownTypeErrors.ts")]
#[case("unmatchedParameterPositions.ts")]
#[case("unparenthesizedConstructorTypeInUnionOrIntersection.ts")]
#[case("unparenthesizedFunctionTypeInUnionOrIntersection.ts")]
#[case("unqualifiedCallToClassStatic1.ts")]
#[case("unreachableFlowAfterFinally.ts")]
#[case("unreachableJavascriptChecked.ts")] // NOT RUNNABLE
#[case("unreachableJavascriptUnchecked.ts")] // NOT RUNNABLE
#[case("unreachableSwitchTypeofAny.ts")] // NOT RUNNABLE
#[case("unreachableSwitchTypeofUnknown.ts")] // NOT RUNNABLE
#[case("unresolvedTypeAssertionSymbol.ts")]
#[case("unspecializedConstraints.ts")]
#[case("unterminatedRegexAtEndOfSource1.ts")]
#[case("unterminatedStringLiteralWithBackslash1.ts")]
#[case("untypedArgumentInLambdaExpression.ts")]
#[case("untypedFunctionCallsWithTypeParameters1.ts")]
#[case("untypedModuleImport_withAugmentation2.ts")] // NOT RUNNABLE
#[case("unusedClassesinModule1.ts")]
#[case("unusedClassesinNamespace1.ts")]
#[case("unusedClassesinNamespace2.ts")]
#[case("unusedClassesinNamespace3.ts")]
#[case("unusedClassesinNamespace4.ts")]
#[case("unusedClassesinNamespace5.ts")]
#[case("unusedDestructuring.ts")]
#[case("unusedDestructuringParameters.ts")]
#[case("unusedFunctionsinNamespaces1.ts")]
#[case("unusedFunctionsinNamespaces2.ts")]
#[case("unusedFunctionsinNamespaces3.ts")]
#[case("unusedFunctionsinNamespaces4.ts")]
#[case("unusedFunctionsinNamespaces5.ts")]
#[case("unusedFunctionsinNamespaces6.ts")]
#[case("unusedGetterInClass.ts")]
#[case("unusedIdentifiersConsolidated1.ts")]
#[case("unusedImportDeclaration.ts")] // NOT RUNNABLE
#[case("unusedImportWithSpread.ts")] // NOT RUNNABLE
#[case("unusedImports1.ts")] // NOT RUNNABLE
#[case("unusedImports10.ts")]
#[case("unusedImports11.ts")] // NOT RUNNABLE
#[case("unusedImports12.ts")] // NOT RUNNABLE
#[case("unusedImports13.ts")] // NOT RUNNABLE
#[case("unusedImports14.ts")] // NOT RUNNABLE
#[case("unusedImports15.ts")] // NOT RUNNABLE
#[case("unusedImports16.ts")] // NOT RUNNABLE
#[case("unusedImports2.ts")] // NOT RUNNABLE
#[case("unusedImports3.ts")] // NOT RUNNABLE
#[case("unusedImports4.ts")] // NOT RUNNABLE
#[case("unusedImports5.ts")] // NOT RUNNABLE
#[case("unusedImports6.ts")] // NOT RUNNABLE
#[case("unusedImports7.ts")] // NOT RUNNABLE
#[case("unusedImports8.ts")] // NOT RUNNABLE
#[case("unusedImports9.ts")] // NOT RUNNABLE
#[case("unusedImports_entireImportDeclaration.ts")] // NOT RUNNABLE
#[case("unusedInterfaceinNamespace1.ts")]
#[case("unusedInterfaceinNamespace2.ts")]
#[case("unusedInterfaceinNamespace3.ts")]
#[case("unusedInterfaceinNamespace4.ts")]
#[case("unusedInterfaceinNamespace5.ts")]
#[case("unusedInvalidTypeArguments.ts")] // NOT RUNNABLE
#[case("unusedLocalProperty.ts")]
#[case("unusedLocalsAndObjectSpread.ts")]
#[case("unusedLocalsAndObjectSpread2.ts")]
#[case("unusedLocalsAndParameters.ts")]
#[case("unusedLocalsAndParametersDeferred.ts")]
#[case("unusedLocalsAndParametersOverloadSignatures.ts")]
#[case("unusedLocalsAndParametersTypeAliases.ts")]
#[case("unusedLocalsAndParametersTypeAliases2.ts")]
#[case("unusedLocalsInMethod1.ts")]
#[case("unusedLocalsInMethod2.ts")]
#[case("unusedLocalsInMethod3.ts")]
#[case("unusedLocalsOnFunctionDeclarationWithinFunctionDeclaration1.ts")]
#[case("unusedLocalsOnFunctionDeclarationWithinFunctionDeclaration2.ts")]
#[case("unusedLocalsOnFunctionDeclarationWithinFunctionExpression1.ts")]
#[case("unusedLocalsOnFunctionDeclarationWithinFunctionExpression2.ts")]
#[case("unusedLocalsOnFunctionExpressionWithinFunctionDeclaration1.ts")]
#[case("unusedLocalsOnFunctionExpressionWithinFunctionDeclaration2.ts")]
#[case("unusedLocalsOnFunctionExpressionWithinFunctionExpression1.ts")]
#[case("unusedLocalsOnFunctionExpressionWithinFunctionExpression2.ts")]
#[case("unusedLocalsStartingWithUnderscore.ts")] // NOT RUNNABLE
#[case("unusedLocalsinConstructor1.ts")]
#[case("unusedLocalsinConstructor2.ts")]
#[case("unusedMethodsInInterface.ts")]
#[case("unusedModuleInModule.ts")]
#[case("unusedMultipleParameter1InContructor.ts")]
#[case("unusedMultipleParameter1InFunctionExpression.ts")]
#[case("unusedMultipleParameter2InContructor.ts")]
#[case("unusedMultipleParameter2InFunctionExpression.ts")]
#[case("unusedMultipleParameters1InFunctionDeclaration.ts")]
#[case("unusedMultipleParameters1InMethodDeclaration.ts")]
#[case("unusedMultipleParameters2InFunctionDeclaration.ts")]
#[case("unusedMultipleParameters2InMethodDeclaration.ts")]
#[case("unusedNamespaceInModule.ts")]
#[case("unusedNamespaceInNamespace.ts")]
#[case("unusedParameterInCatchClause.ts")]
#[case("unusedParameterProperty1.ts")]
#[case("unusedParameterProperty2.ts")]
#[case("unusedParameterUsedInTypeOf.ts")]
#[case("unusedParametersInLambda1.ts")]
#[case("unusedParametersInLambda2.ts")]
#[case("unusedParametersThis.ts")]
#[case("unusedParametersWithUnderscore.ts")]
#[case("unusedParametersinConstructor1.ts")]
#[case("unusedParametersinConstructor2.ts")]
#[case("unusedParametersinConstructor3.ts")]
#[case("unusedPrivateMembers.ts")]
#[case("unusedPrivateMethodInClass1.ts")]
#[case("unusedPrivateMethodInClass2.ts")]
#[case("unusedPrivateMethodInClass3.ts")]
#[case("unusedPrivateMethodInClass4.ts")]
#[case("unusedPrivateStaticMembers.ts")]
#[case("unusedPrivateVariableInClass1.ts")]
#[case("unusedPrivateVariableInClass2.ts")]
#[case("unusedPrivateVariableInClass3.ts")]
#[case("unusedPrivateVariableInClass4.ts")]
#[case("unusedPrivateVariableInClass5.ts")]
#[case("unusedSemicolonInClass.ts")]
#[case("unusedSetterInClass.ts")]
#[case("unusedSetterInClass2.ts")]
#[case("unusedSingleParameterInContructor.ts")]
#[case("unusedSingleParameterInFunctionDeclaration.ts")]
#[case("unusedSingleParameterInFunctionExpression.ts")]
#[case("unusedSingleParameterInMethodDeclaration.ts")]
#[case("unusedSwitchStatement.ts")]
#[case("unusedTypeParameterInFunction1.ts")]
#[case("unusedTypeParameterInFunction2.ts")]
#[case("unusedTypeParameterInFunction3.ts")]
#[case("unusedTypeParameterInFunction4.ts")]
#[case("unusedTypeParameterInInterface1.ts")]
#[case("unusedTypeParameterInInterface2.ts")]
#[case("unusedTypeParameterInLambda1.ts")]
#[case("unusedTypeParameterInLambda2.ts")]
#[case("unusedTypeParameterInLambda3.ts")]
#[case("unusedTypeParameterInMethod1.ts")]
#[case("unusedTypeParameterInMethod2.ts")]
#[case("unusedTypeParameterInMethod3.ts")]
#[case("unusedTypeParameterInMethod4.ts")]
#[case("unusedTypeParameterInMethod5.ts")]
#[case("unusedTypeParameters1.ts")]
#[case("unusedTypeParameters10.ts")]
#[case("unusedTypeParameters2.ts")]
#[case("unusedTypeParameters3.ts")]
#[case("unusedTypeParameters4.ts")]
#[case("unusedTypeParameters5.ts")]
#[case("unusedTypeParameters6.ts")] // NOT RUNNABLE
#[case("unusedTypeParameters7.ts")] // NOT RUNNABLE
#[case("unusedTypeParameters8.ts")] // NOT RUNNABLE
#[case("unusedTypeParameters9.ts")]
#[case("unusedTypeParametersCheckedByNoUnusedParameters.ts")]
#[case("unusedTypeParametersNotCheckedByNoUnusedLocals.ts")]
#[case("unusedTypeParametersWithUnderscore.ts")]
#[case("unusedTypeParameters_infer.ts")]
#[case("unusedTypeParameters_templateTag.ts")] // NOT RUNNABLE
#[case("unusedTypeParameters_templateTag2.ts")] // NOT RUNNABLE
#[case("unusedVariablesWithUnderscoreInBindingElement.ts")]
#[case("unusedVariablesWithUnderscoreInForOfLoop.ts")]
#[case("unusedVariablesinBlocks1.ts")]
#[case("unusedVariablesinBlocks2.ts")]
#[case("unusedVariablesinForLoop.ts")]
#[case("unusedVariablesinForLoop2.ts")]
#[case("unusedVariablesinForLoop3.ts")]
#[case("unusedVariablesinForLoop4.ts")]
#[case("unusedVariablesinModules1.ts")]
#[case("unusedVariablesinNamespaces1.ts")]
#[case("unusedVariablesinNamespaces2.ts")]
#[case("unusedVariablesinNamespaces3.ts")]
#[case("unwitnessedTypeParameterVariance.ts")]
#[case("useBeforeDeclaration.ts")] // NOT RUNNABLE
#[case("useBeforeDeclaration_destructuring.ts")]
#[case("useBeforeDeclaration_jsx.tsx")]
#[case("useBeforeDeclaration_propertyAssignment.ts")]
#[case("useBeforeDeclaration_superClass.ts")]
#[case("useBeforeDefinitionInDeclarationFiles.ts")] // NOT RUNNABLE
#[case("useDefineForClassFieldsFlagDefault.ts")] // NOT RUNNABLE
#[case("useStrictLikePrologueString01.ts")]
#[case("useUnknownInCatchVariables01.ts")]
#[case("usedImportNotElidedInJs.ts")] // NOT RUNNABLE
#[case("usingModuleWithExportImportInValuePosition.ts")]
#[case("validRegexp.ts")]
#[case("validUseOfThisInSuper.ts")]
#[case("valueOfTypedArray.ts")]
#[case("varAndFunctionShareName.ts")]
#[case("varArgConstructorMemberParameter.ts")]
#[case("varArgParamTypeCheck.ts")]
#[case("varArgWithNoParamName.ts")]
#[case("varArgsOnConstructorTypes.ts")]
#[case("varAsID.ts")]
#[case("varBlock.ts")]
#[case("varInFunctionInVarInitializer.ts")]
#[case("varNameConflictsWithImportInDifferentPartOfModule.ts")]
#[case("vararg.ts")]
#[case("vardecl.ts")]
#[case("variableDeclarationDeclarationEmitUniqueSymbolPartialStatement.ts")]
#[case("variableDeclarationInStrictMode1.ts")]
#[case("variableDeclarationInnerCommentEmit.ts")]
#[case("variableDeclarator1.ts")]
#[case("variableDeclaratorResolvedDuringContextualTyping.ts")]
#[case("varianceCallbacksAndIndexedAccesses.ts")]
#[case("varianceMeasurement.ts")]
#[case("varianceProblingAndZeroOrderIndexSignatureRelationsAlign.ts")]
#[case("varianceProblingAndZeroOrderIndexSignatureRelationsAlign2.ts")]
#[case("varianceRepeatedlyPropegatesWithUnreliableFlag.ts")]
#[case("verifyDefaultLib_dom.ts")]
#[case("verifyDefaultLib_webworker.ts")]
#[case("visSyntax.ts")]
#[case("visibilityOfCrossModuleTypeUsage.ts")] // NOT RUNNABLE
#[case("visibilityOfTypeParameters.ts")]
#[case("voidArrayLit.ts")]
#[case("voidAsNonAmbiguousReturnType.ts")] // NOT RUNNABLE
#[case("voidAsOperator.ts")]
#[case("voidConstructor.ts")]
#[case("voidFunctionAssignmentCompat.ts")]
#[case("voidIsInitialized.ts")]
#[case("voidOperator1.ts")]
#[case("voidReturnIndexUnionInference.ts")]
#[case("voidReturnLambdaValue.ts")]
#[case("voidUndefinedReduction.ts")]
#[case("vueLikeDataAndPropsInference.ts")]
#[case("vueLikeDataAndPropsInference2.ts")]
#[case("weakType.ts")]
#[case("webworkerIterable.ts")]
#[case("wellKnownSymbolExpando.ts")]
#[case("whileStatementInnerComments.ts")]
#[case("widenToAny1.ts")]
#[case("widenToAny2.ts")]
#[case("widenedTypes.ts")]
#[case("widenedTypes1.ts")]
#[case("withExportDecl.ts")]
#[case("withImportDecl.ts")] // NOT RUNNABLE
#[case("withStatement.ts")]
#[case("withStatementErrors.ts")]
#[case("withStatementInternalComments.ts")]
#[case("withStatementNestedScope.ts")]
#[case("wrappedIncovations1.ts")]
#[case("wrappedIncovations2.ts")]
#[case("wrappedRecursiveGenericType.ts")]
#[case("yieldExpression1.ts")]
#[case("yieldExpressionInFlowLoop.ts")]
#[case("yieldExpressionInnerCommentEmit.ts")]
#[case("yieldStringLiteral.ts")]
fn run_compiler_baseline(#[case] case_filename: &str) {
    let case_file_path = format!("typescript_src/tests/cases/compiler/{case_filename}");
    let case_file_contents = read_file_and_strip_leading_byte_order_mark(&case_file_path).unwrap();
    let compiler_settings = extract_compiler_settings(&case_file_contents);
    let mut options: CompilerOptions = CompilerOptionsBuilder::default()
        .no_resolve(Some(false))
        .build()
        .unwrap();
    set_compiler_options_from_harness_settings(&compiler_settings, &mut options);
    options.target = Some(get_emit_script_target(&options));
    options.new_line = Some(
        options
            .new_line
            .unwrap_or(NewLineKind::CarriageReturnLineFeed),
    );
    options.no_error_truncation = Some(true);
    options.skip_default_lib_check = Some(options.skip_default_lib_check.unwrap_or(true));
    let options = Rc::new(options);
    let host = create_compiler_host_worker(options.clone(), None, Some(get_sys()));
    let program = create_program(CreateProgramOptions {
        root_names: vec![case_file_path],
        options: options.clone(),
        host: Some(Rc::new(host)),
        project_references: None,
        old_program: None,
        config_file_parsing_diagnostics: None,
    });
    // let emit_result = program.emit(None, None, None, None, None, None);
    let errors = get_pre_emit_diagnostics(&program.clone().into(), Option::<&Node>::None, None);
    compare_baselines(
        Path::new(case_filename)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap(),
        &errors,
        &case_file_contents,
        options.pretty,
    )
}

fn set_compiler_options_from_harness_settings(
    settings: &HashMap<&str, &str>,
    options: &mut CompilerOptions,
) {
    for (&name, &value) in settings {
        let option = get_command_line_option(name);
        if let Some(option) = option.as_ref() {
            let mut errors: Vec<Gc<Diagnostic>> = vec![];
            options.set_value_from_command_line_option(
                option,
                option_value(option, value, &mut errors),
            );
            if !errors.is_empty() {
                panic!("Unknown value '{}' for compiler option '{}'.", value, name);
            }
        } else {
            panic!("Unknown compiler option '{}'.", name);
        }
    }
}

fn option_value(
    option: &CommandLineOption,
    value: &str,
    errors: &mut Vec<Gc<Diagnostic>>,
) -> CompilerOptionsValue {
    match option.type_() {
        // CommandLineOptionType::Boolean => Some(value.to_lowercase() == "true").into(),
        CommandLineOptionType::Boolean => Some(match &*value.to_lowercase() {
            "true" => true,
            "false" => false,
            _ => panic!("Unexpected boolean value: {:?}", value),
        })
        .into(),
        CommandLineOptionType::String => Some(value.to_owned()).into(),
        CommandLineOptionType::Number => unimplemented!(),
        CommandLineOptionType::Object => unimplemented!(),
        CommandLineOptionType::List => {
            CompilerOptionsValue::VecString(parse_list_type_option(option, Some(value), errors))
        }
        CommandLineOptionType::Map(_) => parse_custom_type_option(option, Some(value), errors),
    }
}

thread_local! {
    static options_index: RefCell<Option<HashMap<String, Rc<CommandLineOption>>>> = RefCell::new(None);
}
fn get_command_line_option(name: &str) -> Option<Rc<CommandLineOption>> {
    options_index.with(|options_index_| {
        options_index_
            .borrow_mut()
            .get_or_insert_with(|| {
                let mut options_index_ = HashMap::new();
                option_declarations.with(|option_declarations_| {
                    for option in option_declarations_ {
                        options_index_.insert(option.name().to_lowercase(), option.clone());
                    }
                });
                options_index_
            })
            .get(&name.to_lowercase())
            .cloned()
    })
}

lazy_static! {
    static ref line_ending_regex: Regex = Regex::new(r"\r?\n|\r").unwrap();
    static ref option_regex: Regex = Regex::new(r"^[/]{2}\s*@(\w+)\s*:\s*([^\r\n]*)").unwrap();
}

fn extract_compiler_settings(case_file_contents: &str) -> HashMap<&str, &str> {
    let mut opts: HashMap<&str, &str> = HashMap::new();

    for line in line_ending_regex.split(case_file_contents) {
        if let Some(match_) = option_regex.captures(line) {
            opts.insert(
                match_.get(1).unwrap().as_str(),
                match_.get(2).unwrap().as_str(),
            );
        }
    }

    opts
}

fn compare_baselines(
    name: &str,
    diagnostics: &[Gc<Diagnostic>],
    case_file_contents: &str,
    pretty: Option<bool>,
) {
    let ref baseline_file_contents = fs::read_to_string(&format!(
        "typescript_src/tests/baselines/reference/{name}.errors.txt"
    ))
    .unwrap_or_else(|_| "".to_owned());
    let baseline_error_lines = parse_baseline_errors(baseline_file_contents);
    let formatted_diagnostic_lines = adjust_diagnostic_line_numbers_and_lib_file_paths(
        &if pretty == Some(true) {
            format_diagnostics_with_color_and_context(diagnostics, &DummyFormatDiagnosticsHost)
        } else {
            format_diagnostics(diagnostics, &DummyFormatDiagnosticsHost)
        },
        case_file_contents,
    );
    assert_str_eq!(baseline_error_lines, formatted_diagnostic_lines);
}

fn adjust_diagnostic_line_numbers_and_lib_file_paths(
    formatted_diagnostics: &str,
    case_file_contents: &str,
) -> String {
    let number_of_leading_lines_to_remove =
        get_number_of_leading_lines_to_remove(case_file_contents);
    lazy_static! {
        static ref formatted_diagnostic_regex: Regex =
            Regex::new(r"^(typescript_src/tests/cases/compiler/[^(]+\()(\d+)(,\d+\))").unwrap();
        static ref lib_path_regex: Regex =
            Regex::new(r"^[.\w/]*TypeScript/built/local/(.+)$").unwrap();
    }
    formatted_diagnostics
        .split("\n")
        .map(|line| {
            formatted_diagnostic_regex.replace(line, |captures: &Captures| {
                format!(
                    "{}{}{}",
                    &captures[1],
                    (captures.get(2).unwrap().as_str().parse::<usize>().unwrap()
                        - number_of_leading_lines_to_remove),
                    &captures[3],
                )
            })
        })
        .map(|line| {
            lib_path_regex
                .replace(&line, |captures: &Captures| format!("{}", &captures[1],))
                .into_owned()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

lazy_static! {
    static ref whitespace_only_regex: Regex = Regex::new(r"^\s*$").unwrap();
}

fn get_number_of_leading_lines_to_remove(case_file_contents: &str) -> usize {
    let lines = case_file_contents.split("\n");
    let mut number_of_leading_lines = 0;
    let lines_after_leading = lines.skip_while(|&line| {
        let should_remove = option_regex.is_match(line) || whitespace_only_regex.is_match(line);
        if should_remove {
            number_of_leading_lines += 1;
        }
        should_remove
    });
    let number_of_lines_after_some_actual_file_contents_but_presumably_before_the_line_numbers_of_any_diagnostics =
        option_regex
            .find_iter(&Itertools::intersperse(lines_after_leading, "\n").collect::<String>())
            .count();
    number_of_leading_lines + number_of_lines_after_some_actual_file_contents_but_presumably_before_the_line_numbers_of_any_diagnostics
}

struct DummyFormatDiagnosticsHost;

impl FormatDiagnosticsHost for DummyFormatDiagnosticsHost {
    fn get_current_directory(&self) -> String {
        "".to_owned()
    }

    fn get_new_line(&self) -> &str {
        "\n"
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        file_name.to_owned()
    }
}

fn parse_baseline_errors(baseline_file_contents: &str) -> String {
    lazy_static! {
        static ref delimiter_line_regex: Regex = Regex::new(r"(?m)^(?:====|!!!) ").unwrap();
    }

    if baseline_file_contents.is_empty() {
        return "".to_owned();
    }

    let delimiter_line_start_pos = delimiter_line_regex
        .find(baseline_file_contents)
        .unwrap()
        .start();

    let lines_until_delimiter_line = baseline_file_contents[0..delimiter_line_start_pos]
        .split("\n")
        .collect::<Vec<_>>();
    let lines_until_delimiter_line_reversed = lines_until_delimiter_line
        .into_iter()
        .rev()
        .skip_while(|line| {
            lazy_static! {
                static ref blank_line_regex: Regex = Regex::new(r"^\s*$").unwrap();
            }
            blank_line_regex.is_match(line)
        })
        .collect::<Vec<_>>();
    lines_until_delimiter_line_reversed
        .into_iter()
        .rev()
        .map(|line| {
            line.replace("tests/cases", "typescript_src/tests/cases")
                .replace("\r", "\n")
        })
        .collect::<Vec<_>>()
        .join("")
}
