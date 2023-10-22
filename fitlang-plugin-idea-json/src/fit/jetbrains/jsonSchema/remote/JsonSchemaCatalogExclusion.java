// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.remote;

import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

/**
 * Disables applying schemastore.org JSON schema mappings for particular files.
 */
public interface JsonSchemaCatalogExclusion {
  ExtensionPointName<JsonSchemaCatalogExclusion> EP_NAME = ExtensionPointName.create("fit.intellij.json.catalog.exclusion");

  boolean isExcluded(@NotNull VirtualFile file);
}
