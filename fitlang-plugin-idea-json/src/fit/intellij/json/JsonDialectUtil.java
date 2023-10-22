// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json;

import com.intellij.lang.Language;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.ObjectUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class JsonDialectUtil {
  public static boolean isStandardJson(@NotNull PsiElement element) {
    return isStandardJson(getLanguageOrDefaultJson(element));
  }

  @NotNull
  public static Language getLanguageOrDefaultJson(@NotNull PsiElement element) {
    PsiFile file = element.getContainingFile();
    if (file != null) {
      Language language = file.getLanguage();
      if (language instanceof fit.intellij.json.JsonLanguage) return language;
    }
    return ObjectUtils.coalesce(ObjectUtils.tryCast(element.getLanguage(), fit.intellij.json.JsonLanguage.class), fit.intellij.json.JsonLanguage.INSTANCE);
  }

  public static boolean isStandardJson(@Nullable Language language) {
    return language == JsonLanguage.INSTANCE;
  }
}
