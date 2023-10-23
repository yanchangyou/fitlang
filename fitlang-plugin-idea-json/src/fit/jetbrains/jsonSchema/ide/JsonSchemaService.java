// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.ide;

import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.extension.JsonSchemaFileProvider;
import fit.jetbrains.jsonSchema.extension.JsonSchemaInfo;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaVersion;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

public interface JsonSchemaService {
  class Impl {
    public static JsonSchemaService get(@NotNull Project project) {
      return ServiceManager.getService(project, JsonSchemaService.class);
    }
  }

  static boolean isSchemaFile(@NotNull PsiFile psiFile) {
//    if (JsonLikePsiWalker.getWalker(psiFile, fit.jetbrains.jsonSchema.impl.JsonSchemaObject.NULL_OBJ) == null) return false;
//    final VirtualFile file = psiFile.getViewProvider().getVirtualFile();
//    JsonSchemaService service = Impl.get(psiFile.getProject());
//    return service.isSchemaFile(file) && service.isApplicableToFile(file);
      //TODO
    return false;
  }

  boolean isSchemaFile(@NotNull VirtualFile file);
  boolean isSchemaFile(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schemaObject);

  @NotNull Project getProject();

  @Nullable
  JsonSchemaVersion getSchemaVersion(@NotNull VirtualFile file);

  @NotNull
  Collection<VirtualFile> getSchemaFilesForFile(@NotNull VirtualFile file);

  void registerRemoteUpdateCallback(Runnable callback);
  void unregisterRemoteUpdateCallback(Runnable callback);
  void registerResetAction(Runnable action);
  void unregisterResetAction(Runnable action);

  void registerReference(String ref);
  boolean possiblyHasReference(String ref);

  void triggerUpdateRemote();

  @Nullable
  fit.jetbrains.jsonSchema.impl.JsonSchemaObject getSchemaObject(@NotNull VirtualFile file);

  @Nullable
  fit.jetbrains.jsonSchema.impl.JsonSchemaObject getSchemaObject(@NotNull PsiFile file);

  @Nullable
  fit.jetbrains.jsonSchema.impl.JsonSchemaObject getSchemaObjectForSchemaFile(@NotNull VirtualFile schemaFile);

  @Nullable
  VirtualFile findSchemaFileByReference(@NotNull String reference, @Nullable VirtualFile referent);

  @Nullable
  JsonSchemaFileProvider getSchemaProvider(@NotNull final VirtualFile schemaFile);

  @Nullable
  JsonSchemaFileProvider getSchemaProvider(@NotNull final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schemaObject);

  @Nullable
  VirtualFile resolveSchemaFile(@NotNull final JsonSchemaObject schemaObject);

  void reset();

  List<JsonSchemaInfo> getAllUserVisibleSchemas();

  boolean isApplicableToFile(@Nullable VirtualFile file);
}
