// This is a generated file. Not intended for manual editing.
package fit.intellij.json;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import fit.intellij.json.psi.impl.JsonMyKeywordImpl;
import fit.intellij.json.psi.impl.JsonReferenceExpressionImpl;

public interface JsonElementTypes {

  IElementType ARRAY = new fit.intellij.json.JsonElementType("ARRAY");
  IElementType BOOLEAN_LITERAL = new fit.intellij.json.JsonElementType("BOOLEAN_LITERAL");
  IElementType LITERAL = new fit.intellij.json.JsonElementType("LITERAL");
  IElementType MY_KEYWORD = new JsonElementType("MY_KEYWORD");
  IElementType NULL_LITERAL = new fit.intellij.json.JsonElementType("NULL_LITERAL");
  IElementType NUMBER_LITERAL = new fit.intellij.json.JsonElementType("NUMBER_LITERAL");
  IElementType OBJECT = new fit.intellij.json.JsonElementType("OBJECT");
  IElementType PROPERTY = new fit.intellij.json.JsonElementType("PROPERTY");
  IElementType REFERENCE_EXPRESSION = new fit.intellij.json.JsonElementType("REFERENCE_EXPRESSION");
  IElementType STRING_LITERAL = new fit.intellij.json.JsonElementType("STRING_LITERAL");
  IElementType VALUE = new JsonElementType("VALUE");

  IElementType BLOCK_COMMENT = new fit.intellij.json.JsonTokenType("BLOCK_COMMENT");
  IElementType COLON = new fit.intellij.json.JsonTokenType(":");
  IElementType COMMA = new fit.intellij.json.JsonTokenType(",");
  IElementType DOUBLE_QUOTED_STRING = new fit.intellij.json.JsonTokenType("DOUBLE_QUOTED_STRING");
  IElementType FALSE = new fit.intellij.json.JsonTokenType("false");
  IElementType IDENTIFIER = new fit.intellij.json.JsonTokenType("IDENTIFIER");
  IElementType LINE_COMMENT = new fit.intellij.json.JsonTokenType("LINE_COMMENT");
  IElementType L_BRACKET = new fit.intellij.json.JsonTokenType("[");
  IElementType L_CURLY = new fit.intellij.json.JsonTokenType("{");
  IElementType NULL = new fit.intellij.json.JsonTokenType("null");
  IElementType NUMBER = new fit.intellij.json.JsonTokenType("NUMBER");
  IElementType R_BRACKET = new fit.intellij.json.JsonTokenType("]");
  IElementType R_CURLY = new fit.intellij.json.JsonTokenType("}");
  IElementType SINGLE_QUOTED_STRING = new fit.intellij.json.JsonTokenType("SINGLE_QUOTED_STRING");
  IElementType TRUE = new JsonTokenType("true");
  IElementType UNI = new JsonTokenType("\"uni\"");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == ARRAY) {
        return new fit.intellij.json.psi.impl.JsonArrayImpl(node);
      }
      else if (type == BOOLEAN_LITERAL) {
        return new fit.intellij.json.psi.impl.JsonBooleanLiteralImpl(node);
      }
      else if (type == NULL_LITERAL) {
        return new fit.intellij.json.psi.impl.JsonNullLiteralImpl(node);
      }
      else if (type == NUMBER_LITERAL) {
        return new fit.intellij.json.psi.impl.JsonNumberLiteralImpl(node);
      }
      else if (type == MY_KEYWORD) {
        return new JsonMyKeywordImpl(node);
      }
      else if (type == OBJECT) {
        return new fit.intellij.json.psi.impl.JsonObjectImpl(node);
      }
      else if (type == PROPERTY) {
        return new fit.intellij.json.psi.impl.JsonPropertyImpl(node);
      }
      else if (type == REFERENCE_EXPRESSION) {
        return new JsonReferenceExpressionImpl(node);
      }
      else if (type == STRING_LITERAL) {
        return new fit.intellij.json.psi.impl.JsonStringLiteralImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
