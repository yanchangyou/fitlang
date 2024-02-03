// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi;

import java.util.List;

import org.jetbrains.annotations.*;

public interface JsonPathPathExpression extends JsonPathExpression {

  @Nullable
  JsonPathEvalSegment getEvalSegment();

  @NotNull
  List<JsonPathExpressionSegment> getExpressionSegmentList();

  @NotNull
  List<JsonPathFunctionCall> getFunctionCallList();

  @NotNull
  List<JsonPathIdSegment> getIdSegmentList();

  @Nullable
  JsonPathRootSegment getRootSegment();

  @NotNull
  List<JsonPathWildcardSegment> getWildcardSegmentList();

}
