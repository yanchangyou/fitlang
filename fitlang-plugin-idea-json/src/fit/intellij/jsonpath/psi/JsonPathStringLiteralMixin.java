// Copyright 2000-2021 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.jsonpath.psi;

import com.intellij.lang.ASTNode;
import fit.intellij.jsonpath.psi.impl.JsonPathLiteralValueImpl;
import org.jetbrains.annotations.NotNull;

public abstract class JsonPathStringLiteralMixin extends JsonPathLiteralValueImpl {

  public JsonPathStringLiteralMixin(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void subtreeChanged() {
    putUserData(JsonPathPsiUtils.STRING_FRAGMENTS, null);
  }
}
