// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface JsonPathExpressionSegment extends PsiElement {

  @Nullable
  JsonPathFilterExpression getFilterExpression();

  @Nullable
  JsonPathIndexExpression getIndexExpression();

  @Nullable
  JsonPathIndexesList getIndexesList();

  @Nullable
  JsonPathQuotedPathsList getQuotedPathsList();

  @Nullable
  JsonPathSliceExpression getSliceExpression();

  @Nullable
  JsonPathWildcardSegment getWildcardSegment();

}
