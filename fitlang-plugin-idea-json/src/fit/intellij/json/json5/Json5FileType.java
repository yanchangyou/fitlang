// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.json5;

import fit.intellij.json.JsonFileType;
import org.jetbrains.annotations.NotNull;

public final class Json5FileType extends JsonFileType {
  public static final Json5FileType INSTANCE = new Json5FileType();
  public static final String DEFAULT_EXTENSION = "FitLang5";

  private Json5FileType() {
    super(Json5Language.INSTANCE);
  }

  @NotNull
  @Override
  public String getName() {
    return "FitLang5";
  }

  @NotNull
  @Override
  public String getDescription() {
    return "FitLang5";
  }

  @NotNull
  @Override
  public String getDefaultExtension() {
    return DEFAULT_EXTENSION;
  }
}
