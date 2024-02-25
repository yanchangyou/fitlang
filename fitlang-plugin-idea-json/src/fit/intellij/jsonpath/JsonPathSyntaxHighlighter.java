// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.jsonpath;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import fit.intellij.jsonpath.lexer.JsonPathLexer;
import fit.intellij.jsonpath.psi.JsonPathTypes;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

final class JsonPathSyntaxHighlighter extends SyntaxHighlighterBase {
  public static final TextAttributesKey JSONPATH_KEYWORD =
    createTextAttributesKey("JSONPATH.KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);

  public static final TextAttributesKey JSONPATH_IDENTIFIER =
    createTextAttributesKey("JSONPATH.IDENTIFIER", DefaultLanguageHighlighterColors.INSTANCE_FIELD);

  public static final TextAttributesKey JSONPATH_CONTEXT =
    createTextAttributesKey("JSONPATH.CONTEXT", DefaultLanguageHighlighterColors.STATIC_FIELD);

  public static final TextAttributesKey JSONPATH_OPERATIONS =
    createTextAttributesKey("JSONPATH.OPERATIONS", DefaultLanguageHighlighterColors.OPERATION_SIGN);

  public static final TextAttributesKey JSONPATH_NUMBER =
    createTextAttributesKey("JSONPATH.NUMBER", DefaultLanguageHighlighterColors.NUMBER);

  public static final TextAttributesKey JSONPATH_BOOLEAN =
    createTextAttributesKey("JSONPATH.BOOLEAN", DefaultLanguageHighlighterColors.NUMBER);

  public static final TextAttributesKey JSONPATH_STRING =
    createTextAttributesKey("JSONPATH.STRING", DefaultLanguageHighlighterColors.STRING);

  public static final TextAttributesKey JSONPATH_PARENTHESES =
    createTextAttributesKey("JSONPATH.PARENTHESES", DefaultLanguageHighlighterColors.PARENTHESES);

  public static final TextAttributesKey JSONPATH_BRACKETS =
    createTextAttributesKey("JSONPATH.BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);

  public static final TextAttributesKey JSONPATH_BRACES = // todo braces in object literals
    createTextAttributesKey("JSONPATH.BRACES", DefaultLanguageHighlighterColors.BRACES);

  public static final TextAttributesKey JSONPATH_COMMA =
    createTextAttributesKey("JSONPATH.COMMA", DefaultLanguageHighlighterColors.COMMA);

  public static final TextAttributesKey JSONPATH_DOT =
    createTextAttributesKey("JSONPATH.DOT", DefaultLanguageHighlighterColors.DOT);

  public static final TextAttributesKey JSONPATH_COLON =
    createTextAttributesKey("JSONPATH.COLON", DefaultLanguageHighlighterColors.COMMA);

  public static final TextAttributesKey JSONPATH_FUNCTION_CALL =
    createTextAttributesKey("JSONPATH.FUNCTION", DefaultLanguageHighlighterColors.INSTANCE_METHOD);

  private static final Map<IElementType, TextAttributesKey> ourMap;

  static {
    ourMap = new HashMap<>();

    fillMap(ourMap, JSONPATH_KEYWORD,
            fit.intellij.jsonpath.psi.JsonPathTypes.WILDCARD, fit.intellij.jsonpath.psi.JsonPathTypes.FILTER_OPERATOR, fit.intellij.jsonpath.psi.JsonPathTypes.NULL, fit.intellij.jsonpath.psi.JsonPathTypes.NAMED_OP);
    fillMap(ourMap, JSONPATH_IDENTIFIER,
            fit.intellij.jsonpath.psi.JsonPathTypes.IDENTIFIER);
    fillMap(ourMap, JSONPATH_CONTEXT,
            fit.intellij.jsonpath.psi.JsonPathTypes.ROOT_CONTEXT, fit.intellij.jsonpath.psi.JsonPathTypes.EVAL_CONTEXT);
    fillMap(ourMap, JSONPATH_BRACKETS,
            fit.intellij.jsonpath.psi.JsonPathTypes.LBRACKET, fit.intellij.jsonpath.psi.JsonPathTypes.RBRACKET);
    fillMap(ourMap, JSONPATH_BRACES,
            fit.intellij.jsonpath.psi.JsonPathTypes.LBRACE, fit.intellij.jsonpath.psi.JsonPathTypes.RBRACE);
    fillMap(ourMap, JSONPATH_PARENTHESES,
            fit.intellij.jsonpath.psi.JsonPathTypes.LPARENTH, fit.intellij.jsonpath.psi.JsonPathTypes.RPARENTH);
    fillMap(ourMap, JSONPATH_DOT,
            fit.intellij.jsonpath.psi.JsonPathTypes.DOT, fit.intellij.jsonpath.psi.JsonPathTypes.RECURSIVE_DESCENT);
    fillMap(ourMap, JSONPATH_COMMA,
            fit.intellij.jsonpath.psi.JsonPathTypes.COMMA);
    fillMap(ourMap, JSONPATH_COLON,
            fit.intellij.jsonpath.psi.JsonPathTypes.COLON);

    fillMap(ourMap, JSONPATH_NUMBER,
            fit.intellij.jsonpath.psi.JsonPathTypes.INTEGER_NUMBER, fit.intellij.jsonpath.psi.JsonPathTypes.DOUBLE_NUMBER);
    fillMap(ourMap, JSONPATH_BOOLEAN,
            fit.intellij.jsonpath.psi.JsonPathTypes.TRUE, fit.intellij.jsonpath.psi.JsonPathTypes.FALSE);
    fillMap(ourMap, JSONPATH_STRING,
            fit.intellij.jsonpath.psi.JsonPathTypes.SINGLE_QUOTED_STRING, fit.intellij.jsonpath.psi.JsonPathTypes.DOUBLE_QUOTED_STRING, fit.intellij.jsonpath.psi.JsonPathTypes.REGEX_STRING);

    fillMap(ourMap, JSONPATH_OPERATIONS,
            fit.intellij.jsonpath.psi.JsonPathTypes.OR_OP, fit.intellij.jsonpath.psi.JsonPathTypes.AND_OP,
            fit.intellij.jsonpath.psi.JsonPathTypes.NOT_OP, fit.intellij.jsonpath.psi.JsonPathTypes.EQ_OP, fit.intellij.jsonpath.psi.JsonPathTypes.NE_OP, fit.intellij.jsonpath.psi.JsonPathTypes.RE_OP,
            fit.intellij.jsonpath.psi.JsonPathTypes.GT_OP, fit.intellij.jsonpath.psi.JsonPathTypes.LT_OP, fit.intellij.jsonpath.psi.JsonPathTypes.GE_OP, fit.intellij.jsonpath.psi.JsonPathTypes.LE_OP,
            fit.intellij.jsonpath.psi.JsonPathTypes.MINUS_OP, fit.intellij.jsonpath.psi.JsonPathTypes.PLUS_OP, fit.intellij.jsonpath.psi.JsonPathTypes.MULTIPLY_OP, JsonPathTypes.DIVIDE_OP);
  }

  @Override
  public @NotNull Lexer getHighlightingLexer() {
    return new JsonPathLexer();
  }

  @Override
  public TextAttributesKey @NotNull [] getTokenHighlights(IElementType tokenType) {
    return pack(ourMap.get(tokenType));
  }
}
