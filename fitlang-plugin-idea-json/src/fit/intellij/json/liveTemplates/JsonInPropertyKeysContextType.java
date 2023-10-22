// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.liveTemplates;

import com.intellij.codeInsight.template.TemplateContextType;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.JsonElementTypes;
import fit.intellij.json.psi.JsonValue;
import com.intellij.patterns.PatternCondition;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.ProcessingContext;
import fit.intellij.json.psi.JsonFile;
import fit.intellij.json.psi.JsonPsiUtil;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class JsonInPropertyKeysContextType extends TemplateContextType {
  protected JsonInPropertyKeysContextType() {
    super("JSON_PROPERTY_KEYS", JsonBundle.message("json.property.keys"), JsonContextType.class);
  }

  @Override
  public boolean isInContext(@NotNull PsiFile file, int offset) {
    return file instanceof JsonFile && psiElement().inside(psiElement(JsonValue.class)
                                                             .with(new PatternCondition<PsiElement>("insidePropertyKey") {
                                                               @Override
                                                               public boolean accepts(@NotNull PsiElement element,
                                                                                      ProcessingContext context) {
                                                                 return JsonPsiUtil.isPropertyKey(element);
                                                               }
                                                             })).beforeLeaf(psiElement(JsonElementTypes.COLON)).accepts(file.findElementAt(offset));
  }
}