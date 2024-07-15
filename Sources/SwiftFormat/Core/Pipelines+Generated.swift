//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file is automatically generated with generate-swift-format. Do not edit!

import SwiftSyntax

/// A syntax visitor that delegates to individual rules for linting.
///
/// This file will be extended with `visit` methods in Pipelines+Generated.swift.
class LintPipeline: SyntaxVisitor {

  /// The formatter context.
  let context: Context

  /// Stores lint and format rule instances, indexed by the `ObjectIdentifier` of a rule's
  /// class type.
  var ruleCache = [ObjectIdentifier: Rule]()

  /// Rules present in this dictionary skip visiting children until they leave the
  /// syntax node stored as their value
  var shouldSkipChildren = [ObjectIdentifier: SyntaxProtocol]()

  /// Creates a new lint pipeline.
  init(context: Context) {
    self.context = context
    super.init(viewMode: .sourceAccurate)
  }

  override func visit(_ node: ActorDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ActorDeclSyntax) {    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
  }

  override func visit(_ node: AsExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NeverForceUnwrap.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: AsExprSyntax) {    onVisitPost(rule: NeverForceUnwrap.self, for: node)
  }

  override func visit(_ node: AssociatedTypeDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: AssociatedTypeDeclSyntax) {    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
  }

  override func visit(_ node: AttributeSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AvoidRetroactiveConformances.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: AttributeSyntax) {
    onVisitPost(rule: AvoidRetroactiveConformances.self, for: node)
  }

  override func visit(_ node: ClassDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(DontRepeatTypeInStaticProperties.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ClassDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: DontRepeatTypeInStaticProperties.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: ClosureExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(OmitExplicitReturns.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ClosureExprSyntax) {    onVisitPost(rule: OmitExplicitReturns.self, for: node)
  }

  override func visit(_ node: ClosureParameterSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ClosureParameterSyntax) {    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: ClosureSignatureSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    visitIfEnabled(ReturnVoidInsteadOfEmptyTuple.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ClosureSignatureSyntax) {    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
    onVisitPost(rule: ReturnVoidInsteadOfEmptyTuple.self, for: node)
  }

  override func visit(_ node: CodeBlockItemListSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(DoNotUseSemicolons.visit, for: node)
    visitIfEnabled(NoAssignmentInExpressions.visit, for: node)
    visitIfEnabled(OneVariableDeclarationPerLine.visit, for: node)
    visitIfEnabled(UseEarlyExits.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: CodeBlockItemListSyntax) {    onVisitPost(rule: DoNotUseSemicolons.self, for: node)
    onVisitPost(rule: NoAssignmentInExpressions.self, for: node)
    onVisitPost(rule: OneVariableDeclarationPerLine.self, for: node)
    onVisitPost(rule: UseEarlyExits.self, for: node)
  }

  override func visit(_ node: CodeBlockSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AmbiguousTrailingClosureOverload.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: CodeBlockSyntax) {    onVisitPost(rule: AmbiguousTrailingClosureOverload.self, for: node)
  }

  override func visit(_ node: ConditionElementSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoParensAroundConditions.visit, for: node)
    visitIfEnabled(UseExplicitNilCheckInConditions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ConditionElementSyntax) {    onVisitPost(rule: NoParensAroundConditions.self, for: node)
    onVisitPost(rule: UseExplicitNilCheckInConditions.self, for: node)
  }

  override func visit(_ node: DeinitializerDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: DeinitializerDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: EnumCaseElementSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: EnumCaseElementSyntax) {    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: EnumCaseParameterSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: EnumCaseParameterSyntax) {    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: EnumDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(DontRepeatTypeInStaticProperties.visit, for: node)
    visitIfEnabled(FullyIndirectEnum.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(OneCasePerLine.visit, for: node)
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: EnumDeclSyntax) {    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: DontRepeatTypeInStaticProperties.self, for: node)
    onVisitPost(rule: FullyIndirectEnum.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: OneCasePerLine.self, for: node)
    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: ExtensionDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AvoidRetroactiveConformances.visit, for: node)
    visitIfEnabled(DontRepeatTypeInStaticProperties.visit, for: node)
    visitIfEnabled(NoAccessLevelOnExtensionDeclaration.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ExtensionDeclSyntax) {
    onVisitPost(rule: AvoidRetroactiveConformances.self, for: node)
    onVisitPost(rule: DontRepeatTypeInStaticProperties.self, for: node)
    onVisitPost(rule: NoAccessLevelOnExtensionDeclaration.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: ForStmtSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(UseWhereClausesInForLoops.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ForStmtSyntax) {    onVisitPost(rule: UseWhereClausesInForLoops.self, for: node)
  }

  override func visit(_ node: ForceUnwrapExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NeverForceUnwrap.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ForceUnwrapExprSyntax) {    onVisitPost(rule: NeverForceUnwrap.self, for: node)
  }

  override func visit(_ node: FunctionCallExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoEmptyTrailingClosureParentheses.visit, for: node)
    visitIfEnabled(OnlyOneTrailingClosureArgument.visit, for: node)
    visitIfEnabled(ReplaceForEachWithForLoop.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: FunctionCallExprSyntax) {    onVisitPost(rule: NoEmptyTrailingClosureParentheses.self, for: node)
    onVisitPost(rule: OnlyOneTrailingClosureArgument.self, for: node)
    onVisitPost(rule: ReplaceForEachWithForLoop.self, for: node)
  }

  override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(OmitExplicitReturns.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    visitIfEnabled(ValidateDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: FunctionDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: OmitExplicitReturns.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
    onVisitPost(rule: ValidateDocumentationComments.self, for: node)
  }

  override func visit(_ node: FunctionParameterSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AlwaysUseLiteralForEmptyCollectionInit.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: FunctionParameterSyntax) {    onVisitPost(rule: AlwaysUseLiteralForEmptyCollectionInit.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: FunctionSignatureSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoVoidReturnOnFunctionSignature.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: FunctionSignatureSyntax) {    onVisitPost(rule: NoVoidReturnOnFunctionSignature.self, for: node)
  }

  override func visit(_ node: FunctionTypeSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(ReturnVoidInsteadOfEmptyTuple.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: FunctionTypeSyntax) {    onVisitPost(rule: ReturnVoidInsteadOfEmptyTuple.self, for: node)
  }

  override func visit(_ node: GenericParameterSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: GenericParameterSyntax) {    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: GenericSpecializationExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(UseShorthandTypeNames.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: GenericSpecializationExprSyntax) {    onVisitPost(rule: UseShorthandTypeNames.self, for: node)
  }

  override func visit(_ node: GuardStmtSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoParensAroundConditions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: GuardStmtSyntax) {    onVisitPost(rule: NoParensAroundConditions.self, for: node)
  }

  override func visit(_ node: IdentifierPatternSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(IdentifiersMustBeASCII.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: IdentifierPatternSyntax) {    onVisitPost(rule: IdentifiersMustBeASCII.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: IdentifierTypeSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(UseShorthandTypeNames.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: IdentifierTypeSyntax) {    onVisitPost(rule: UseShorthandTypeNames.self, for: node)
  }

  override func visit(_ node: IfExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoParensAroundConditions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: IfExprSyntax) {    onVisitPost(rule: NoParensAroundConditions.self, for: node)
  }

  override func visit(_ node: InfixOperatorExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoAssignmentInExpressions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: InfixOperatorExprSyntax) {    onVisitPost(rule: NoAssignmentInExpressions.self, for: node)
  }

  override func visit(_ node: InitializerDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    visitIfEnabled(ValidateDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: InitializerDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
    onVisitPost(rule: ValidateDocumentationComments.self, for: node)
  }

  override func visit(_ node: IntegerLiteralExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(GroupNumericLiterals.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: IntegerLiteralExprSyntax) {    onVisitPost(rule: GroupNumericLiterals.self, for: node)
  }

  override func visit(_ node: MacroExpansionExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoPlaygroundLiterals.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: MacroExpansionExprSyntax) {    onVisitPost(rule: NoPlaygroundLiterals.self, for: node)
  }

  override func visit(_ node: MemberBlockItemListSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(DoNotUseSemicolons.visit, for: node)
    visitIfEnabled(OrderedDeclarations.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: MemberBlockItemListSyntax) {    onVisitPost(rule: DoNotUseSemicolons.self, for: node)
    onVisitPost(rule: OrderedDeclarations.self, for: node)
  }

  override func visit(_ node: MemberBlockSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AmbiguousTrailingClosureOverload.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: MemberBlockSyntax) {    onVisitPost(rule: AmbiguousTrailingClosureOverload.self, for: node)
  }

  override func visit(_ node: OptionalBindingConditionSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: OptionalBindingConditionSyntax) {    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
  }

  override func visit(_ node: PatternBindingSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AlwaysUseLiteralForEmptyCollectionInit.visit, for: node)
    visitIfEnabled(OmitExplicitReturns.visit, for: node)
    visitIfEnabled(UseSingleLinePropertyGetter.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: PatternBindingSyntax) {    onVisitPost(rule: AlwaysUseLiteralForEmptyCollectionInit.self, for: node)
    onVisitPost(rule: OmitExplicitReturns.self, for: node)
    onVisitPost(rule: UseSingleLinePropertyGetter.self, for: node)
  }

  override func visit(_ node: PrecedenceGroupDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: PrecedenceGroupDeclSyntax) {    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
  }

  override func visit(_ node: ProtocolDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(DontRepeatTypeInStaticProperties.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ProtocolDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: DontRepeatTypeInStaticProperties.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: RepeatStmtSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoParensAroundConditions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: RepeatStmtSyntax) {    onVisitPost(rule: NoParensAroundConditions.self, for: node)
  }

  override func visit(_ node: SourceFileSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    visitIfEnabled(AmbiguousTrailingClosureOverload.visit, for: node)
    visitIfEnabled(FileScopedDeclarationPrivacy.visit, for: node)
    visitIfEnabled(NeverForceUnwrap.visit, for: node)
    visitIfEnabled(NeverUseForceTry.visit, for: node)
    visitIfEnabled(NeverUseImplicitlyUnwrappedOptionals.visit, for: node)
    visitIfEnabled(OrderedImports.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: SourceFileSyntax) {    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
    onVisitPost(rule: AmbiguousTrailingClosureOverload.self, for: node)
    onVisitPost(rule: FileScopedDeclarationPrivacy.self, for: node)
    onVisitPost(rule: NeverForceUnwrap.self, for: node)
    onVisitPost(rule: NeverUseForceTry.self, for: node)
    onVisitPost(rule: NeverUseImplicitlyUnwrappedOptionals.self, for: node)
    onVisitPost(rule: OrderedImports.self, for: node)
  }

  override func visit(_ node: StructDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(DontRepeatTypeInStaticProperties.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    visitIfEnabled(UseSynthesizedInitializer.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: StructDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: DontRepeatTypeInStaticProperties.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
    onVisitPost(rule: UseSynthesizedInitializer.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: SubscriptDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(OmitExplicitReturns.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: SubscriptDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: OmitExplicitReturns.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: SwitchCaseLabelSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoLabelsInCasePatterns.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: SwitchCaseLabelSyntax) {    onVisitPost(rule: NoLabelsInCasePatterns.self, for: node)
  }

  override func visit(_ node: SwitchCaseListSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoCasesWithOnlyFallthrough.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: SwitchCaseListSyntax) {    onVisitPost(rule: NoCasesWithOnlyFallthrough.self, for: node)
  }

  override func visit(_ node: SwitchExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoParensAroundConditions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: SwitchExprSyntax) {    onVisitPost(rule: NoParensAroundConditions.self, for: node)
  }

  override func visit(_ node: TokenSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoBlockComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: TokenSyntax) {    onVisitPost(rule: NoBlockComments.self, for: node)
  }

  override func visit(_ node: TryExprSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NeverUseForceTry.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: TryExprSyntax) {    onVisitPost(rule: NeverUseForceTry.self, for: node)
  }

  override func visit(_ node: TypeAliasDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(NoLeadingUnderscores.visit, for: node)
    visitIfEnabled(TypeNamesShouldBeCapitalized.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: TypeAliasDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: NoLeadingUnderscores.self, for: node)
    onVisitPost(rule: TypeNamesShouldBeCapitalized.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: ValueBindingPatternSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(UseLetInEveryBoundCaseVariable.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: ValueBindingPatternSyntax) {    onVisitPost(rule: UseLetInEveryBoundCaseVariable.self, for: node)
  }

  override func visit(_ node: VariableDeclSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(AllPublicDeclarationsHaveDocumentation.visit, for: node)
    visitIfEnabled(AlwaysUseLowerCamelCase.visit, for: node)
    visitIfEnabled(BeginDocumentationCommentWithOneLineSummary.visit, for: node)
    visitIfEnabled(NeverUseImplicitlyUnwrappedOptionals.visit, for: node)
    visitIfEnabled(UseTripleSlashForDocumentationComments.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: VariableDeclSyntax) {    onVisitPost(rule: AllPublicDeclarationsHaveDocumentation.self, for: node)
    onVisitPost(rule: AlwaysUseLowerCamelCase.self, for: node)
    onVisitPost(rule: BeginDocumentationCommentWithOneLineSummary.self, for: node)
    onVisitPost(rule: NeverUseImplicitlyUnwrappedOptionals.self, for: node)
    onVisitPost(rule: UseTripleSlashForDocumentationComments.self, for: node)
  }

  override func visit(_ node: WhileStmtSyntax) -> SyntaxVisitorContinueKind {
    visitIfEnabled(NoParensAroundConditions.visit, for: node)
    return .visitChildren
  }
  override func visitPost(_ node: WhileStmtSyntax) {    onVisitPost(rule: NoParensAroundConditions.self, for: node)
  }
}

extension FormatPipeline {

  func rewrite(_ node: Syntax) -> Syntax {
    var node = node
    node = AlwaysUseLiteralForEmptyCollectionInit(context: context).rewrite(node)
    node = DoNotUseSemicolons(context: context).rewrite(node)
    node = FileScopedDeclarationPrivacy(context: context).rewrite(node)
    node = FullyIndirectEnum(context: context).rewrite(node)
    node = GroupNumericLiterals(context: context).rewrite(node)
    node = NoAccessLevelOnExtensionDeclaration(context: context).rewrite(node)
    node = NoAssignmentInExpressions(context: context).rewrite(node)
    node = NoCasesWithOnlyFallthrough(context: context).rewrite(node)
    node = NoEmptyTrailingClosureParentheses(context: context).rewrite(node)
    node = NoLabelsInCasePatterns(context: context).rewrite(node)
    node = NoParensAroundConditions(context: context).rewrite(node)
    node = NoVoidReturnOnFunctionSignature(context: context).rewrite(node)
    node = OmitExplicitReturns(context: context).rewrite(node)
    node = OneCasePerLine(context: context).rewrite(node)
    node = OneVariableDeclarationPerLine(context: context).rewrite(node)
    node = OrderedDeclarations(context: context).rewrite(node)
    node = OrderedImports(context: context).rewrite(node)
    node = ReturnVoidInsteadOfEmptyTuple(context: context).rewrite(node)
    node = UseEarlyExits(context: context).rewrite(node)
    node = UseExplicitNilCheckInConditions(context: context).rewrite(node)
    node = UseShorthandTypeNames(context: context).rewrite(node)
    node = UseSingleLinePropertyGetter(context: context).rewrite(node)
    node = UseTripleSlashForDocumentationComments(context: context).rewrite(node)
    node = UseWhereClausesInForLoops(context: context).rewrite(node)
    return node
  }
}
