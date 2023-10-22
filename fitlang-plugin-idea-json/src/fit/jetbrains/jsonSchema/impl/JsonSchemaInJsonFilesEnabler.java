// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import fit.intellij.json.JsonUtil;
import com.intellij.openapi.vfs.VirtualFile;
import fit.jetbrains.jsonSchema.extension.JsonSchemaEnabler;

public class JsonSchemaInJsonFilesEnabler implements JsonSchemaEnabler {
  @Override
  public boolean isEnabledForFile(VirtualFile file) {
    return JsonUtil.isJsonFile(file);
  }

  @Override
  public boolean canBeSchemaFile(VirtualFile file) {
    return isEnabledForFile(file);
  }
}
