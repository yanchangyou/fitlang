// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.text.StringUtil;
import fit.intellij.json.JsonBundle;

import javax.swing.*;

public enum JsonMappingKind {
  File,
  Pattern,
  Directory;

  public String getDescription() {
    switch (this) {
      case File:
        return fit.intellij.json.JsonBundle.message("schema.mapping.file");
      case Pattern:
        return fit.intellij.json.JsonBundle.message("schema.mapping.pattern");
      case Directory:
        return JsonBundle.message("schema.mapping.directory");
    }
    return "";
  }

  public String getPrefix() {
    return StringUtil.capitalize(getDescription()) + ": ";
  }

  public Icon getIcon() {
    switch (this) {
      case File:
        return AllIcons.FileTypes.Any_type;
      case Pattern:
        return AllIcons.FileTypes.Unknown;
      case Directory:
        return AllIcons.Nodes.Folder;
    }
    return null;
  }
}
