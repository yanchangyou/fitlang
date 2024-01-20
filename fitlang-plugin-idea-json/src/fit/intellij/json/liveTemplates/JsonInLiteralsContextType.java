// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.liveTemplates;

import com.intellij.codeInsight.template.TemplateContextType;
import fit.intellij.json.psi.JsonFile;
import com.intellij.psi.PsiFile;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.psi.JsonStringLiteral;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class JsonInLiteralsContextType extends TemplateContextType {
  protected JsonInLiteralsContextType() {
    super(JsonBundle.message("json.string.values"));
  }

  @Override
  public boolean isInContext(@NotNull PsiFile file, int offset) {
    return file instanceof JsonFile && psiElement().inside(JsonStringLiteral.class).accepts(file.findElementAt(offset));
  }
}
