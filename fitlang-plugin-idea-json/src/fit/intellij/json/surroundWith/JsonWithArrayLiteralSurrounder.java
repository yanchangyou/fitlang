// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.surroundWith;

import fit.intellij.json.JsonBundle;
import org.jetbrains.annotations.NotNull;

public class JsonWithArrayLiteralSurrounder extends JsonSurrounderBase {
  @Override
  public String getTemplateDescription() {
    return JsonBundle.message("surround.with.array.literal.desc");
  }

  @NotNull
  @Override
  protected String createReplacementText(@NotNull String firstElement) {
    return "[" + firstElement + "]";
  }
}
