// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import fit.intellij.jsonpath.psi.impl.JsonPathEvalSegmentImpl;

public interface JsonPathTypes {

  IElementType AND_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("AND_EXPRESSION");
  IElementType ARRAY_VALUE = new fit.intellij.jsonpath.psi.JsonPathElementType("ARRAY_VALUE");
  IElementType BINARY_CONDITIONAL_OPERATOR = new fit.intellij.jsonpath.psi.JsonPathElementType("BINARY_CONDITIONAL_OPERATOR");
  IElementType BOOLEAN_LITERAL = new fit.intellij.jsonpath.psi.JsonPathElementType("BOOLEAN_LITERAL");
  IElementType CONDITIONAL_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("CONDITIONAL_EXPRESSION");
  IElementType DIVIDE_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("DIVIDE_EXPRESSION");
  IElementType EVAL_SEGMENT = new fit.intellij.jsonpath.psi.JsonPathElementType("EVAL_SEGMENT");
  IElementType EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("EXPRESSION");
  IElementType EXPRESSION_SEGMENT = new fit.intellij.jsonpath.psi.JsonPathElementType("EXPRESSION_SEGMENT");
  IElementType FILTER_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("FILTER_EXPRESSION");
  IElementType FUNCTION_ARGS_LIST = new fit.intellij.jsonpath.psi.JsonPathElementType("FUNCTION_ARGS_LIST");
  IElementType FUNCTION_CALL = new fit.intellij.jsonpath.psi.JsonPathElementType("FUNCTION_CALL");
  IElementType ID = new fit.intellij.jsonpath.psi.JsonPathElementType("ID");
  IElementType ID_SEGMENT = new fit.intellij.jsonpath.psi.JsonPathElementType("ID_SEGMENT");
  IElementType INDEXES_LIST = new fit.intellij.jsonpath.psi.JsonPathElementType("INDEXES_LIST");
  IElementType INDEX_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("INDEX_EXPRESSION");
  IElementType LITERAL_VALUE = new fit.intellij.jsonpath.psi.JsonPathElementType("LITERAL_VALUE");
  IElementType MINUS_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("MINUS_EXPRESSION");
  IElementType MULTIPLY_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("MULTIPLY_EXPRESSION");
  IElementType NULL_LITERAL = new fit.intellij.jsonpath.psi.JsonPathElementType("NULL_LITERAL");
  IElementType NUMBER_LITERAL = new fit.intellij.jsonpath.psi.JsonPathElementType("NUMBER_LITERAL");
  IElementType OBJECT_PROPERTY = new fit.intellij.jsonpath.psi.JsonPathElementType("OBJECT_PROPERTY");
  IElementType OBJECT_VALUE = new fit.intellij.jsonpath.psi.JsonPathElementType("OBJECT_VALUE");
  IElementType OR_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("OR_EXPRESSION");
  IElementType PARENTHESIZED_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("PARENTHESIZED_EXPRESSION");
  IElementType PATH_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("PATH_EXPRESSION");
  IElementType PLUS_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("PLUS_EXPRESSION");
  IElementType QUOTED_PATHS_LIST = new fit.intellij.jsonpath.psi.JsonPathElementType("QUOTED_PATHS_LIST");
  IElementType REGEX_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("REGEX_EXPRESSION");
  IElementType REGEX_LITERAL = new fit.intellij.jsonpath.psi.JsonPathElementType("REGEX_LITERAL");
  IElementType ROOT_SEGMENT = new fit.intellij.jsonpath.psi.JsonPathElementType("ROOT_SEGMENT");
  IElementType SLICE_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("SLICE_EXPRESSION");
  IElementType STRING_LITERAL = new fit.intellij.jsonpath.psi.JsonPathElementType("STRING_LITERAL");
  IElementType UNARY_MINUS_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("UNARY_MINUS_EXPRESSION");
  IElementType UNARY_NOT_EXPRESSION = new fit.intellij.jsonpath.psi.JsonPathElementType("UNARY_NOT_EXPRESSION");
  IElementType VALUE = new fit.intellij.jsonpath.psi.JsonPathElementType("VALUE");
  IElementType WILDCARD_SEGMENT = new JsonPathElementType("WILDCARD_SEGMENT");

  IElementType AND_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("AND_OP");
  IElementType COLON = new fit.intellij.jsonpath.psi.JsonPathTokenType(":");
  IElementType COMMA = new fit.intellij.jsonpath.psi.JsonPathTokenType(",");
  IElementType DIVIDE_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("DIVIDE_OP");
  IElementType DOT = new fit.intellij.jsonpath.psi.JsonPathTokenType(".");
  IElementType DOUBLE_NUMBER = new fit.intellij.jsonpath.psi.JsonPathTokenType("DOUBLE_NUMBER");
  IElementType DOUBLE_QUOTED_STRING = new fit.intellij.jsonpath.psi.JsonPathTokenType("DOUBLE_QUOTED_STRING");
  IElementType EEQ_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("EEQ_OP");
  IElementType ENE_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("ENE_OP");
  IElementType EQ_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("EQ_OP");
  IElementType EVAL_CONTEXT = new fit.intellij.jsonpath.psi.JsonPathTokenType("@");
  IElementType FALSE = new fit.intellij.jsonpath.psi.JsonPathTokenType("false");
  IElementType FILTER_OPERATOR = new fit.intellij.jsonpath.psi.JsonPathTokenType("?");
  IElementType GE_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("GE_OP");
  IElementType GT_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("GT_OP");
  IElementType IDENTIFIER = new fit.intellij.jsonpath.psi.JsonPathTokenType("IDENTIFIER");
  IElementType INTEGER_NUMBER = new fit.intellij.jsonpath.psi.JsonPathTokenType("INTEGER_NUMBER");
  IElementType LBRACE = new fit.intellij.jsonpath.psi.JsonPathTokenType("{");
  IElementType LBRACKET = new fit.intellij.jsonpath.psi.JsonPathTokenType("[");
  IElementType LE_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("LE_OP");
  IElementType LPARENTH = new fit.intellij.jsonpath.psi.JsonPathTokenType("(");
  IElementType LT_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("LT_OP");
  IElementType MINUS_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("MINUS_OP");
  IElementType MULTIPLY_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("MULTIPLY_OP");
  IElementType NAMED_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("NAMED_OP");
  IElementType NE_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("NE_OP");
  IElementType NOT_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("NOT_OP");
  IElementType NULL = new fit.intellij.jsonpath.psi.JsonPathTokenType("null");
  IElementType OR_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("OR_OP");
  IElementType PLUS_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("PLUS_OP");
  IElementType RBRACE = new fit.intellij.jsonpath.psi.JsonPathTokenType("}");
  IElementType RBRACKET = new fit.intellij.jsonpath.psi.JsonPathTokenType("]");
  IElementType RECURSIVE_DESCENT = new fit.intellij.jsonpath.psi.JsonPathTokenType("..");
  IElementType REGEX_STRING = new fit.intellij.jsonpath.psi.JsonPathTokenType("REGEX_STRING");
  IElementType RE_OP = new fit.intellij.jsonpath.psi.JsonPathTokenType("RE_OP");
  IElementType ROOT_CONTEXT = new fit.intellij.jsonpath.psi.JsonPathTokenType("$");
  IElementType RPARENTH = new fit.intellij.jsonpath.psi.JsonPathTokenType(")");
  IElementType SINGLE_QUOTED_STRING = new fit.intellij.jsonpath.psi.JsonPathTokenType("SINGLE_QUOTED_STRING");
  IElementType TRUE = new fit.intellij.jsonpath.psi.JsonPathTokenType("true");
  IElementType WILDCARD = new JsonPathTokenType("*");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == AND_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathAndExpressionImpl(node);
      }
      else if (type == ARRAY_VALUE) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathArrayValueImpl(node);
      }
      else if (type == BINARY_CONDITIONAL_OPERATOR) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathBinaryConditionalOperatorImpl(node);
      }
      else if (type == BOOLEAN_LITERAL) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathBooleanLiteralImpl(node);
      }
      else if (type == CONDITIONAL_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathConditionalExpressionImpl(node);
      }
      else if (type == DIVIDE_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathDivideExpressionImpl(node);
      }
      else if (type == EVAL_SEGMENT) {
        return new JsonPathEvalSegmentImpl(node);
      }
      else if (type == EXPRESSION_SEGMENT) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathExpressionSegmentImpl(node);
      }
      else if (type == FILTER_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathFilterExpressionImpl(node);
      }
      else if (type == FUNCTION_ARGS_LIST) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathFunctionArgsListImpl(node);
      }
      else if (type == FUNCTION_CALL) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathFunctionCallImpl(node);
      }
      else if (type == ID) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathIdImpl(node);
      }
      else if (type == ID_SEGMENT) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathIdSegmentImpl(node);
      }
      else if (type == INDEXES_LIST) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathIndexesListImpl(node);
      }
      else if (type == INDEX_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathIndexExpressionImpl(node);
      }
      else if (type == MINUS_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathMinusExpressionImpl(node);
      }
      else if (type == MULTIPLY_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathMultiplyExpressionImpl(node);
      }
      else if (type == NULL_LITERAL) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathNullLiteralImpl(node);
      }
      else if (type == NUMBER_LITERAL) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathNumberLiteralImpl(node);
      }
      else if (type == OBJECT_PROPERTY) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathObjectPropertyImpl(node);
      }
      else if (type == OBJECT_VALUE) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathObjectValueImpl(node);
      }
      else if (type == OR_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathOrExpressionImpl(node);
      }
      else if (type == PARENTHESIZED_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathParenthesizedExpressionImpl(node);
      }
      else if (type == PATH_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathPathExpressionImpl(node);
      }
      else if (type == PLUS_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathPlusExpressionImpl(node);
      }
      else if (type == QUOTED_PATHS_LIST) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathQuotedPathsListImpl(node);
      }
      else if (type == REGEX_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathRegexExpressionImpl(node);
      }
      else if (type == REGEX_LITERAL) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathRegexLiteralImpl(node);
      }
      else if (type == ROOT_SEGMENT) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathRootSegmentImpl(node);
      }
      else if (type == SLICE_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathSliceExpressionImpl(node);
      }
      else if (type == STRING_LITERAL) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathStringLiteralImpl(node);
      }
      else if (type == UNARY_MINUS_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathUnaryMinusExpressionImpl(node);
      }
      else if (type == UNARY_NOT_EXPRESSION) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathUnaryNotExpressionImpl(node);
      }
      else if (type == WILDCARD_SEGMENT) {
        return new fit.intellij.jsonpath.psi.impl.JsonPathWildcardSegmentImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
