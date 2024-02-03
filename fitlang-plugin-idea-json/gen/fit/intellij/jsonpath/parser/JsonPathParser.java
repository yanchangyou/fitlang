// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;

import static com.intellij.lang.parser.GeneratedParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;
import fit.intellij.jsonpath.psi.JsonPathTypes;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class JsonPathParser implements PsiParser, LightPsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, EXTENDS_SETS_);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    r = parse_root_(t, b);
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b) {
    return parse_root_(t, b, 0);
  }

  static boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    boolean r;
    if (t == JsonPathTypes.EXPRESSION) {
      r = expression(b, l + 1, -1);
    }
    else {
      r = root(b, l + 1);
    }
    return r;
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(JsonPathTypes.AND_EXPRESSION, JsonPathTypes.ARRAY_VALUE, JsonPathTypes.BOOLEAN_LITERAL, JsonPathTypes.CONDITIONAL_EXPRESSION,
      JsonPathTypes.DIVIDE_EXPRESSION, JsonPathTypes.EXPRESSION, JsonPathTypes.LITERAL_VALUE, JsonPathTypes.MINUS_EXPRESSION,
      JsonPathTypes.MULTIPLY_EXPRESSION, JsonPathTypes.NULL_LITERAL, JsonPathTypes.NUMBER_LITERAL, JsonPathTypes.OBJECT_VALUE,
      JsonPathTypes.OR_EXPRESSION, JsonPathTypes.PARENTHESIZED_EXPRESSION, JsonPathTypes.PATH_EXPRESSION, JsonPathTypes.PLUS_EXPRESSION,
      JsonPathTypes.REGEX_EXPRESSION, JsonPathTypes.STRING_LITERAL, JsonPathTypes.UNARY_MINUS_EXPRESSION, JsonPathTypes.UNARY_NOT_EXPRESSION,
      JsonPathTypes.VALUE),
  };

  /* ********************************************************** */
  // value (COMMA|&RBRACKET)
  static boolean arrayElement_(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "arrayElement_")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_);
    r = value(b, l + 1);
    p = r; // pin = 1
    r = r && arrayElement__1(b, l + 1);
    exit_section_(b, l, m, r, p, JsonPathParser::notBracketOrNextValue);
    return r || p;
  }

  // COMMA|&RBRACKET
  private static boolean arrayElement__1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "arrayElement__1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.COMMA);
    if (!r) r = arrayElement__1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &RBRACKET
  private static boolean arrayElement__1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "arrayElement__1_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_);
    r = consumeToken(b, JsonPathTypes.RBRACKET);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // LBRACKET arrayElement_* RBRACKET
  public static boolean arrayValue(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "arrayValue")) return false;
    if (!nextTokenIs(b, JsonPathTypes.LBRACKET)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.LBRACKET);
    r = r && arrayValue_1(b, l + 1);
    r = r && consumeToken(b, JsonPathTypes.RBRACKET);
    exit_section_(b, m, JsonPathTypes.ARRAY_VALUE, r);
    return r;
  }

  // arrayElement_*
  private static boolean arrayValue_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "arrayValue_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!arrayElement_(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "arrayValue_1", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // EEQ_OP | ENE_OP | EQ_OP | NE_OP | GT_OP | LT_OP | GE_OP | LE_OP | NAMED_OP
  public static boolean binaryConditionalOperator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binaryConditionalOperator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.BINARY_CONDITIONAL_OPERATOR, "<operator>");
    r = consumeToken(b, JsonPathTypes.EEQ_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.ENE_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.EQ_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.NE_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.GT_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.LT_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.GE_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.LE_OP);
    if (!r) r = consumeToken(b, JsonPathTypes.NAMED_OP);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // TRUE | FALSE
  public static boolean booleanLiteral(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "booleanLiteral")) return false;
    if (!nextTokenIs(b, "<boolean literal>", JsonPathTypes.FALSE, JsonPathTypes.TRUE)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.BOOLEAN_LITERAL, "<boolean literal>");
    r = consumeToken(b, JsonPathTypes.TRUE);
    if (!r) r = consumeToken(b, JsonPathTypes.FALSE);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // (RECURSIVE_DESCENT | DOT) (functionCall | wildcardSegment | idSegment | expressionSegment)
  static boolean dotSegment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dotSegment")) return false;
    if (!nextTokenIs(b, "", JsonPathTypes.DOT, JsonPathTypes.RECURSIVE_DESCENT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_);
    r = dotSegment_0(b, l + 1);
    p = r; // pin = 1
    r = r && dotSegment_1(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // RECURSIVE_DESCENT | DOT
  private static boolean dotSegment_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dotSegment_0")) return false;
    boolean r;
    r = consumeToken(b, JsonPathTypes.RECURSIVE_DESCENT);
    if (!r) r = consumeToken(b, JsonPathTypes.DOT);
    return r;
  }

  // functionCall | wildcardSegment | idSegment | expressionSegment
  private static boolean dotSegment_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "dotSegment_1")) return false;
    boolean r;
    r = functionCall(b, l + 1);
    if (!r) r = wildcardSegment(b, l + 1);
    if (!r) r = idSegment(b, l + 1);
    if (!r) r = expressionSegment(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // EVAL_CONTEXT
  public static boolean evalSegment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "evalSegment")) return false;
    if (!nextTokenIs(b, JsonPathTypes.EVAL_CONTEXT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.EVAL_CONTEXT);
    exit_section_(b, m, JsonPathTypes.EVAL_SEGMENT, r);
    return r;
  }

  /* ********************************************************** */
  // LBRACKET nestedExpression_ RBRACKET
  public static boolean expressionSegment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expressionSegment")) return false;
    if (!nextTokenIs(b, JsonPathTypes.LBRACKET)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.LBRACKET);
    r = r && nestedExpression_(b, l + 1);
    r = r && consumeToken(b, JsonPathTypes.RBRACKET);
    exit_section_(b, m, JsonPathTypes.EXPRESSION_SEGMENT, r);
    return r;
  }

  /* ********************************************************** */
  // FILTER_OPERATOR (LPARENTH expression RPARENTH)?
  public static boolean filterExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "filterExpression")) return false;
    if (!nextTokenIs(b, JsonPathTypes.FILTER_OPERATOR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.FILTER_EXPRESSION, null);
    r = consumeToken(b, JsonPathTypes.FILTER_OPERATOR);
    p = r; // pin = 1
    r = r && filterExpression_1(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // (LPARENTH expression RPARENTH)?
  private static boolean filterExpression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "filterExpression_1")) return false;
    filterExpression_1_0(b, l + 1);
    return true;
  }

  // LPARENTH expression RPARENTH
  private static boolean filterExpression_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "filterExpression_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.LPARENTH);
    r = r && expression(b, l + 1, -1);
    r = r && consumeToken(b, JsonPathTypes.RPARENTH);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // expression (COMMA expression)*
  public static boolean functionArgsList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "functionArgsList")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.FUNCTION_ARGS_LIST, "<function arguments>");
    r = expression(b, l + 1, -1);
    r = r && functionArgsList_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // (COMMA expression)*
  private static boolean functionArgsList_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "functionArgsList_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!functionArgsList_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "functionArgsList_1", c)) break;
    }
    return true;
  }

  // COMMA expression
  private static boolean functionArgsList_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "functionArgsList_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.COMMA);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // id LPARENTH functionArgsList? RPARENTH
  public static boolean functionCall(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "functionCall")) return false;
    if (!nextTokenIs(b, JsonPathTypes.IDENTIFIER)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.FUNCTION_CALL, null);
    r = id(b, l + 1);
    r = r && consumeToken(b, JsonPathTypes.LPARENTH);
    p = r; // pin = 2
    r = r && report_error_(b, functionCall_2(b, l + 1));
    r = p && consumeToken(b, JsonPathTypes.RPARENTH) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // functionArgsList?
  private static boolean functionCall_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "functionCall_2")) return false;
    functionArgsList(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // rootSegment | evalSegment | wildcardSegment | idSegment | expressionSegment
  static boolean head_(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "head_")) return false;
    boolean r;
    r = rootSegment(b, l + 1);
    if (!r) r = evalSegment(b, l + 1);
    if (!r) r = wildcardSegment(b, l + 1);
    if (!r) r = idSegment(b, l + 1);
    if (!r) r = expressionSegment(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // IDENTIFIER
  public static boolean id(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "id")) return false;
    if (!nextTokenIs(b, "<identifier>", JsonPathTypes.IDENTIFIER)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.ID, "<identifier>");
    r = consumeToken(b, JsonPathTypes.IDENTIFIER);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean idSegment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "idSegment")) return false;
    if (!nextTokenIs(b, JsonPathTypes.IDENTIFIER)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = id(b, l + 1);
    exit_section_(b, m, JsonPathTypes.ID_SEGMENT, r);
    return r;
  }

  /* ********************************************************** */
  // LPARENTH expression RPARENTH
  public static boolean indexExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "indexExpression")) return false;
    if (!nextTokenIs(b, JsonPathTypes.LPARENTH)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.INDEX_EXPRESSION, null);
    r = consumeToken(b, JsonPathTypes.LPARENTH);
    p = r; // pin = 1
    r = r && report_error_(b, expression(b, l + 1, -1));
    r = p && consumeToken(b, JsonPathTypes.RPARENTH) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // INTEGER_NUMBER (COMMA INTEGER_NUMBER)*
  public static boolean indexesList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "indexesList")) return false;
    if (!nextTokenIs(b, "<indexes list>", JsonPathTypes.INTEGER_NUMBER)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.INDEXES_LIST, "<indexes list>");
    r = consumeToken(b, JsonPathTypes.INTEGER_NUMBER);
    r = r && indexesList_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // (COMMA INTEGER_NUMBER)*
  private static boolean indexesList_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "indexesList_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!indexesList_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "indexesList_1", c)) break;
    }
    return true;
  }

  // COMMA INTEGER_NUMBER
  private static boolean indexesList_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "indexesList_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, JsonPathTypes.COMMA, JsonPathTypes.INTEGER_NUMBER);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // nullLiteral | booleanLiteral | numberLiteral | stringLiteral
  public static boolean literalValue(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "literalValue")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, JsonPathTypes.LITERAL_VALUE, "<literalValue>");
    r = nullLiteral(b, l + 1);
    if (!r) r = booleanLiteral(b, l + 1);
    if (!r) r = numberLiteral(b, l + 1);
    if (!r) r = stringLiteral(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // wildcardSegment |
  //   filterExpression | // standalone filter operator may be substituted by programmatic filter
  //   indexExpression | // supported only in some implementations
  //   sliceExpression |
  //   indexesList |
  //   quotedPathsList
  static boolean nestedExpression_(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "nestedExpression_")) return false;
    boolean r;
    r = wildcardSegment(b, l + 1);
    if (!r) r = filterExpression(b, l + 1);
    if (!r) r = indexExpression(b, l + 1);
    if (!r) r = sliceExpression(b, l + 1);
    if (!r) r = indexesList(b, l + 1);
    if (!r) r = quotedPathsList(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // !(RBRACE|value)
  static boolean notBraceOrNextValue(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "notBraceOrNextValue")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_);
    r = !notBraceOrNextValue_0(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // RBRACE|value
  private static boolean notBraceOrNextValue_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "notBraceOrNextValue_0")) return false;
    boolean r;
    r = consumeToken(b, JsonPathTypes.RBRACE);
    if (!r) r = value(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // !(RBRACKET|value)
  static boolean notBracketOrNextValue(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "notBracketOrNextValue")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_);
    r = !notBracketOrNextValue_0(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // RBRACKET|value
  private static boolean notBracketOrNextValue_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "notBracketOrNextValue_0")) return false;
    boolean r;
    r = consumeToken(b, JsonPathTypes.RBRACKET);
    if (!r) r = value(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // NULL
  public static boolean nullLiteral(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "nullLiteral")) return false;
    if (!nextTokenIs(b, "<null>", JsonPathTypes.NULL)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.NULL_LITERAL, "<null>");
    r = consumeToken(b, JsonPathTypes.NULL);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // DOUBLE_NUMBER | INTEGER_NUMBER
  public static boolean numberLiteral(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "numberLiteral")) return false;
    if (!nextTokenIs(b, "<number literal>", JsonPathTypes.DOUBLE_NUMBER, JsonPathTypes.INTEGER_NUMBER)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.NUMBER_LITERAL, "<number literal>");
    r = consumeToken(b, JsonPathTypes.DOUBLE_NUMBER);
    if (!r) r = consumeToken(b, JsonPathTypes.INTEGER_NUMBER);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // objectProperty (COMMA|&RBRACE)
  static boolean objectElement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectElement")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_);
    r = objectProperty(b, l + 1);
    p = r; // pin = 1
    r = r && objectElement_1(b, l + 1);
    exit_section_(b, l, m, r, p, JsonPathParser::notBraceOrNextValue);
    return r || p;
  }

  // COMMA|&RBRACE
  private static boolean objectElement_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectElement_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.COMMA);
    if (!r) r = objectElement_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &RBRACE
  private static boolean objectElement_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectElement_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_);
    r = consumeToken(b, JsonPathTypes.RBRACE);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // stringLiteral (COLON value)
  public static boolean objectProperty(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectProperty")) return false;
    if (!nextTokenIs(b, "<object property>", JsonPathTypes.DOUBLE_QUOTED_STRING, JsonPathTypes.SINGLE_QUOTED_STRING)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.OBJECT_PROPERTY, "<object property>");
    r = stringLiteral(b, l + 1);
    p = r; // pin = 1
    r = r && objectProperty_1(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // COLON value
  private static boolean objectProperty_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectProperty_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_);
    r = consumeToken(b, JsonPathTypes.COLON);
    p = r; // pin = 1
    r = r && value(b, l + 1);
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // LBRACE objectElement* RBRACE
  public static boolean objectValue(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectValue")) return false;
    if (!nextTokenIs(b, JsonPathTypes.LBRACE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.OBJECT_VALUE, null);
    r = consumeToken(b, JsonPathTypes.LBRACE);
    p = r; // pin = 1
    r = r && report_error_(b, objectValue_1(b, l + 1));
    r = p && consumeToken(b, JsonPathTypes.RBRACE) && r;
    exit_section_(b, l, m, r, p, null);
    return r || p;
  }

  // objectElement*
  private static boolean objectValue_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "objectValue_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!objectElement(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "objectValue_1", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // stringLiteral (COMMA stringLiteral)*
  public static boolean quotedPathsList(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quotedPathsList")) return false;
    if (!nextTokenIs(b, "<quoted paths list>", JsonPathTypes.DOUBLE_QUOTED_STRING, JsonPathTypes.SINGLE_QUOTED_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.QUOTED_PATHS_LIST, "<quoted paths list>");
    r = stringLiteral(b, l + 1);
    r = r && quotedPathsList_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // (COMMA stringLiteral)*
  private static boolean quotedPathsList_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quotedPathsList_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!quotedPathsList_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "quotedPathsList_1", c)) break;
    }
    return true;
  }

  // COMMA stringLiteral
  private static boolean quotedPathsList_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quotedPathsList_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.COMMA);
    r = r && stringLiteral(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // REGEX_STRING
  public static boolean regexLiteral(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "regexLiteral")) return false;
    if (!nextTokenIs(b, "<regex literal>", JsonPathTypes.REGEX_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.REGEX_LITERAL, "<regex literal>");
    r = consumeToken(b, JsonPathTypes.REGEX_STRING);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // functionCall | (head_ segments_*)
  static boolean root(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "root")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = functionCall(b, l + 1);
    if (!r) r = root_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // head_ segments_*
  private static boolean root_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "root_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = head_(b, l + 1);
    r = r && root_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // segments_*
  private static boolean root_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "root_1_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!segments_(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "root_1_1", c)) break;
    }
    return true;
  }

  /* ********************************************************** */
  // ROOT_CONTEXT
  public static boolean rootSegment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rootSegment")) return false;
    if (!nextTokenIs(b, JsonPathTypes.ROOT_CONTEXT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, JsonPathTypes.ROOT_CONTEXT);
    exit_section_(b, m, JsonPathTypes.ROOT_SEGMENT, r);
    return r;
  }

  /* ********************************************************** */
  // wildcardSegment | dotSegment | expressionSegment
  static boolean segments_(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "segments_")) return false;
    boolean r;
    r = wildcardSegment(b, l + 1);
    if (!r) r = dotSegment(b, l + 1);
    if (!r) r = expressionSegment(b, l + 1);
    return r;
  }

  /* ********************************************************** */
  // (INTEGER_NUMBER COLON INTEGER_NUMBER COLON INTEGER_NUMBER) |
  //   (INTEGER_NUMBER COLON INTEGER_NUMBER) |
  //   (INTEGER_NUMBER COLON) |
  //   (COLON INTEGER_NUMBER)
  public static boolean sliceExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sliceExpression")) return false;
    if (!nextTokenIs(b, "<slice expression>", JsonPathTypes.COLON, JsonPathTypes.INTEGER_NUMBER)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.SLICE_EXPRESSION, "<slice expression>");
    r = sliceExpression_0(b, l + 1);
    if (!r) r = sliceExpression_1(b, l + 1);
    if (!r) r = sliceExpression_2(b, l + 1);
    if (!r) r = sliceExpression_3(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // INTEGER_NUMBER COLON INTEGER_NUMBER COLON INTEGER_NUMBER
  private static boolean sliceExpression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sliceExpression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, JsonPathTypes.INTEGER_NUMBER, JsonPathTypes.COLON, JsonPathTypes.INTEGER_NUMBER, JsonPathTypes.COLON, JsonPathTypes.INTEGER_NUMBER);
    exit_section_(b, m, null, r);
    return r;
  }

  // INTEGER_NUMBER COLON INTEGER_NUMBER
  private static boolean sliceExpression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sliceExpression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, JsonPathTypes.INTEGER_NUMBER, JsonPathTypes.COLON, JsonPathTypes.INTEGER_NUMBER);
    exit_section_(b, m, null, r);
    return r;
  }

  // INTEGER_NUMBER COLON
  private static boolean sliceExpression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sliceExpression_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, JsonPathTypes.INTEGER_NUMBER, JsonPathTypes.COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  // COLON INTEGER_NUMBER
  private static boolean sliceExpression_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sliceExpression_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, JsonPathTypes.COLON, JsonPathTypes.INTEGER_NUMBER);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // SINGLE_QUOTED_STRING | DOUBLE_QUOTED_STRING
  public static boolean stringLiteral(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "stringLiteral")) return false;
    if (!nextTokenIs(b, "<string literal>", JsonPathTypes.DOUBLE_QUOTED_STRING, JsonPathTypes.SINGLE_QUOTED_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.STRING_LITERAL, "<string literal>");
    r = consumeToken(b, JsonPathTypes.SINGLE_QUOTED_STRING);
    if (!r) r = consumeToken(b, JsonPathTypes.DOUBLE_QUOTED_STRING);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // WILDCARD
  public static boolean wildcardSegment(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "wildcardSegment")) return false;
    if (!nextTokenIs(b, "<*>", JsonPathTypes.WILDCARD)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.WILDCARD_SEGMENT, "<*>");
    r = consumeToken(b, JsonPathTypes.WILDCARD);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // Expression root: expression
  // Operator priority table:
  // 0: PREFIX(unaryNotExpression) PREFIX(unaryMinusExpression)
  // 1: BINARY(andExpression) BINARY(orExpression)
  // 2: BINARY(conditionalExpression) POSTFIX(regexExpression)
  // 3: BINARY(plusExpression) BINARY(minusExpression)
  // 4: BINARY(multiplyExpression) BINARY(divideExpression)
  // 5: ATOM(value) ATOM(pathExpression) PREFIX(parenthesizedExpression)
  public static boolean expression(PsiBuilder b, int l, int g) {
    if (!recursion_guard_(b, l, "expression")) return false;
    addVariant(b, "<expression>");
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = unaryNotExpression(b, l + 1);
    if (!r) r = unaryMinusExpression(b, l + 1);
    if (!r) r = value(b, l + 1);
    if (!r) r = pathExpression(b, l + 1);
    if (!r) r = parenthesizedExpression(b, l + 1);
    p = r;
    r = r && expression_0(b, l + 1, g);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  public static boolean expression_0(PsiBuilder b, int l, int g) {
    if (!recursion_guard_(b, l, "expression_0")) return false;
    boolean r = true;
    while (true) {
      Marker m = enter_section_(b, l, _LEFT_, null);
      if (g < 3 && consumeTokenSmart(b, JsonPathTypes.MINUS_OP)) {
        r = expression(b, l, 3);
        exit_section_(b, l, m, JsonPathTypes.MINUS_EXPRESSION, r, true, null);
      }
      else if (g < 1 && consumeTokenSmart(b, JsonPathTypes.AND_OP)) {
        r = expression(b, l, 1);
        exit_section_(b, l, m, JsonPathTypes.AND_EXPRESSION, r, true, null);
      }
      else if (g < 1 && consumeTokenSmart(b, JsonPathTypes.OR_OP)) {
        r = expression(b, l, 1);
        exit_section_(b, l, m, JsonPathTypes.OR_EXPRESSION, r, true, null);
      }
      else if (g < 2 && binaryConditionalOperator(b, l + 1)) {
        r = expression(b, l, 2);
        exit_section_(b, l, m, JsonPathTypes.CONDITIONAL_EXPRESSION, r, true, null);
      }
      else if (g < 2 && regexExpression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, JsonPathTypes.REGEX_EXPRESSION, r, true, null);
      }
      else if (g < 3 && consumeTokenSmart(b, JsonPathTypes.PLUS_OP)) {
        r = expression(b, l, 3);
        exit_section_(b, l, m, JsonPathTypes.PLUS_EXPRESSION, r, true, null);
      }
      else if (g < 4 && consumeTokenSmart(b, JsonPathTypes.MULTIPLY_OP)) {
        r = expression(b, l, 4);
        exit_section_(b, l, m, JsonPathTypes.MULTIPLY_EXPRESSION, r, true, null);
      }
      else if (g < 4 && consumeTokenSmart(b, JsonPathTypes.DIVIDE_OP)) {
        r = expression(b, l, 4);
        exit_section_(b, l, m, JsonPathTypes.DIVIDE_EXPRESSION, r, true, null);
      }
      else {
        exit_section_(b, l, m, null, false, false, null);
        break;
      }
    }
    return r;
  }

  public static boolean unaryNotExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unaryNotExpression")) return false;
    if (!nextTokenIsSmart(b, JsonPathTypes.NOT_OP)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeTokenSmart(b, JsonPathTypes.NOT_OP);
    p = r;
    r = p && expression(b, l, 0);
    exit_section_(b, l, m, JsonPathTypes.UNARY_NOT_EXPRESSION, r, p, null);
    return r || p;
  }

  public static boolean unaryMinusExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unaryMinusExpression")) return false;
    if (!nextTokenIsSmart(b, JsonPathTypes.MINUS_OP)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeTokenSmart(b, JsonPathTypes.MINUS_OP);
    p = r;
    r = p && expression(b, l, 0);
    exit_section_(b, l, m, JsonPathTypes.UNARY_MINUS_EXPRESSION, r, p, null);
    return r || p;
  }

  // RE_OP regexLiteral
  private static boolean regexExpression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "regexExpression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, JsonPathTypes.RE_OP);
    r = r && regexLiteral(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // objectValue | arrayValue | literalValue
  public static boolean value(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "value")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, JsonPathTypes.VALUE, "<value>");
    r = objectValue(b, l + 1);
    if (!r) r = arrayValue(b, l + 1);
    if (!r) r = literalValue(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // head_ segments_*
  public static boolean pathExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pathExpression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, JsonPathTypes.PATH_EXPRESSION, "<path expression>");
    r = head_(b, l + 1);
    r = r && pathExpression_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // segments_*
  private static boolean pathExpression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "pathExpression_1")) return false;
    while (true) {
      int c = current_position_(b);
      if (!segments_(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "pathExpression_1", c)) break;
    }
    return true;
  }

  public static boolean parenthesizedExpression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesizedExpression")) return false;
    if (!nextTokenIsSmart(b, JsonPathTypes.LPARENTH)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeTokenSmart(b, JsonPathTypes.LPARENTH);
    p = r;
    r = p && expression(b, l, -1);
    r = p && report_error_(b, consumeToken(b, JsonPathTypes.RPARENTH)) && r;
    exit_section_(b, l, m, JsonPathTypes.PARENTHESIZED_EXPRESSION, r, p, null);
    return r || p;
  }

}
