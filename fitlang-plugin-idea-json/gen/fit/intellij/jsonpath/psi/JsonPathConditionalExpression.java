// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi;

import java.util.List;
import org.jetbrains.annotations.*;

public interface JsonPathConditionalExpression extends JsonPathExpression {

  @NotNull
  JsonPathBinaryConditionalOperator getBinaryConditionalOperator();

  @NotNull
  List<JsonPathExpression> getExpressionList();

}
