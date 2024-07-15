//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import Foundation

@_spi(Rules)
public final class OrderedDeclarations: SyntaxFormatRule {
    private enum DeclarationOrder: Int {
        case staticVariable = 0
        case staticFunction = 1
        case nestedType = 2
        case overridenVariable = 3
        case instanceVariable = 4
        case initializer = 5
        case deinitializer = 6
        case view = 7
        case overridenFunction = 8
        case instanceFunction = 9
        case unknown = 10

        var name: String {
            // Convert camel-case to space-separated words
            let name = "\(self)"
            var result = ""
            for char in name {
                if char.isUppercase {
                    result.append(" ")
                }
                result.append(char)
            }
            let lower = result.lowercased()
            // capitalize first letter of first word
            return result.prefix(1).uppercased() + lower.dropFirst()
        }
    }

    private enum ModifierOrder: Int {
        case open = 0
        case `public` = 1
        case `internal` = 2
        case `fileprivate` = 3
        case `private` = 4
    }

    private func getDeclModifierOrder(_ decl: DeclSyntax) -> ModifierOrder {
        guard let typeErased = decl.children(viewMode: .all).first(where: { $0.is(DeclModifierListSyntax.self) }),
              let modifiers = typeErased.as(DeclModifierListSyntax.self) else {
            return .internal
        }

        if modifiers.contains(anyOf: [.open]) {
            return .open
        } else if modifiers.contains(anyOf: [.public]) {
            return .public
        } else if modifiers.contains(anyOf: [.internal]) {
            return .internal
        } else if modifiers.contains(anyOf: [.fileprivate]) {
            return .fileprivate
        } else if modifiers.contains(anyOf: [.private]) {
            return .private
        }

        return .internal
    }

    private func getDeclOrderKind(_ decl: DeclSyntax) -> DeclarationOrder {
        if let asVar = decl.as(VariableDeclSyntax.self) {
            if asVar.modifiers.contains(anyOf: [.static, .class]) {
                return .staticVariable
            } else if asVar.modifiers.contains(anyOf: [.override]) {
                return .overridenVariable
            } else if let typeSyntax = asVar.bindings.first?.typeAnnotation?.type.as(SomeOrAnyTypeSyntax.self),
                        typeSyntax.someOrAnySpecifier.text == "some",
                        let constraint = typeSyntax.constraint.as(IdentifierTypeSyntax.self),
                        constraint.name.text == "View" {
                return .view
            } else {
                return .instanceVariable
            }
        } else if let asFunc = decl.as(FunctionDeclSyntax.self) {
            if asFunc.modifiers.contains(anyOf: [.static, .class]) {
                return .staticFunction
            } else if asFunc.modifiers.contains(anyOf: [.override]) {
                return .overridenFunction
            } else if let typeSyntax = asFunc.signature.returnClause?.type.as(SomeOrAnyTypeSyntax.self),
                        typeSyntax.someOrAnySpecifier.text == "some",
                        let constraint = typeSyntax.constraint.as(IdentifierTypeSyntax.self),
                        constraint.name.text == "View" {
                return .view
            } else {
                return .instanceFunction
            }
        } else if decl.is(InitializerDeclSyntax.self) {
            return .initializer
        } else if decl.is(DeinitializerDeclSyntax.self) {
            return .deinitializer
        } else if decl.is(ClassDeclSyntax.self) || decl.is(StructDeclSyntax.self) || decl.is(EnumDeclSyntax.self) {
            return .nestedType
        } else {
            return .unknown
        }
    }

    public override func visit(_ node: MemberBlockItemListSyntax) -> MemberBlockItemListSyntax {
        let list = super.visit(node)

        var currentOrder = DeclarationOrder.staticVariable
        var currentModifierOrder = ModifierOrder.open

        for child in node {
            let kind = getDeclOrderKind(child.decl)
            let modifierOrder = getDeclModifierOrder(child.decl)
            if kind == .unknown {
                continue
            }

            if kind.rawValue < currentOrder.rawValue {
                diagnose(incorrectOrder(last: currentOrder, new: kind), on: child.decl)
                continue
            }

            if kind.rawValue == currentOrder.rawValue && modifierOrder.rawValue < currentModifierOrder.rawValue {
                diagnose(incorrectModifierOrder(last: currentModifierOrder, new: modifierOrder), on: child.decl)
                continue
            }

            currentOrder = kind
            currentModifierOrder = modifierOrder
        }

        return .init(list.sorted(by: { (lhs, rhs) -> Bool in
            let lhsKind = getDeclOrderKind(lhs.decl)
            let rhsKind = getDeclOrderKind(rhs.decl)
            if lhsKind.rawValue != rhsKind.rawValue {
                return lhsKind.rawValue < rhsKind.rawValue
            }

            let lhsModifierOrder = getDeclModifierOrder(lhs.decl)
            let rhsModifierOrder = getDeclModifierOrder(rhs.decl)
            return lhsModifierOrder.rawValue < rhsModifierOrder.rawValue
        }))
    }

    private func incorrectOrder(last: DeclarationOrder, new: DeclarationOrder) -> Finding.Message {
        return Finding.Message("\(new.name) declarations should precede \(last.name) declarations")
    }

    private func incorrectModifierOrder(last: ModifierOrder, new: ModifierOrder) -> Finding.Message {
        return Finding.Message("\(new) declarations should precede \(last) declarations")
    }
}
