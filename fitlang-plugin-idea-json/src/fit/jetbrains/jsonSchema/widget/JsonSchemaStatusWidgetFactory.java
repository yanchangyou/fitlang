// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
package fit.jetbrains.jsonSchema.widget;

import fit.intellij.json.JsonBundle;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.openapi.wm.impl.status.widget.StatusBarEditorBasedWidgetFactory;
import kotlinx.coroutines.CoroutineScope;
import org.jetbrains.annotations.NotNull;

public final class JsonSchemaStatusWidgetFactory extends StatusBarEditorBasedWidgetFactory {
  @Override
  public @NotNull String getId() {
    return fit.jetbrains.jsonSchema.widget.JsonSchemaStatusWidget.ID;
  }

  @Override
  public @NotNull String getDisplayName() {
    return JsonBundle.message("schema.widget.display.name");
  }

  @Override
  public boolean canBeEnabledOn(@NotNull StatusBar statusBar) {
    Project project = statusBar.getProject();
    if (project == null) {
      return false;
    }

    FileEditor editor = getFileEditor(statusBar);
    return fit.jetbrains.jsonSchema.widget.JsonSchemaStatusWidget.isAvailableOnFile(project, editor != null ? editor.getFile() : null);
  }

  @Override
  public @NotNull StatusBarWidget createWidget(@NotNull Project project, @NotNull CoroutineScope scope) {
    return new fit.jetbrains.jsonSchema.widget.JsonSchemaStatusWidget(project, scope);
  }
}
