// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.psi.impl;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiRecursiveVisitor;
import fit.intellij.json.psi.JsonElementVisitor;
import org.jetbrains.annotations.NotNull;

/**
 * @author Mikhail Golubev
 */
public class JsonRecursiveElementVisitor extends JsonElementVisitor implements PsiRecursiveVisitor {

  @Override
  public void visitElement(@NotNull final PsiElement element) {
    element.acceptChildren(this);
  }
}
