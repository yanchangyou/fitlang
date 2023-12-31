// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.extension;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.PairProcessor;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.JsonSchemaMappingsProjectConfiguration;
import fit.jetbrains.jsonSchema.UserDefinedJsonSchemaConfiguration;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaVersion;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.List;
import java.util.Map;

/**
 * @author Irina.Chernushina on 2/13/2016.
 */
public class JsonSchemaUserDefinedProviderFactory implements JsonSchemaProviderFactory {
  @NotNull
  @Override
  public List<JsonSchemaFileProvider> getProviders(@NotNull Project project) {
    final JsonSchemaMappingsProjectConfiguration configuration = JsonSchemaMappingsProjectConfiguration.getInstance(project);

    final Map<String, UserDefinedJsonSchemaConfiguration> map = configuration.getStateMap();
    final List<JsonSchemaFileProvider> providers = ContainerUtil.map(map.values(), schema -> createProvider(project, schema));

    return providers;
  }

  @NotNull
  public MyProvider createProvider(@NotNull Project project,
                                   UserDefinedJsonSchemaConfiguration schema) {
    String relPath = schema.getRelativePathToSchema();
    return new MyProvider(project, schema.getSchemaVersion(), schema.getName(),
                          fit.jetbrains.jsonSchema.remote.JsonFileResolver.isHttpPath(relPath) || relPath.startsWith(JsonSchemaObject.TEMP_URL) || new File(relPath).isAbsolute()
                            ? relPath
                            : new File(project.getBasePath(),
                          relPath).getAbsolutePath(),
                          schema.getCalculatedPatterns());
  }

  static class MyProvider implements JsonSchemaFileProvider, JsonSchemaImportedProviderMarker {
    @NotNull private final Project myProject;
    @NotNull private final fit.jetbrains.jsonSchema.impl.JsonSchemaVersion myVersion;
    @NotNull private final String myName;
    @NotNull private final String myFile;
    private VirtualFile myVirtualFile;
    @NotNull private final List<? extends PairProcessor<Project, VirtualFile>> myPatterns;

    MyProvider(@NotNull final Project project,
                      @NotNull final fit.jetbrains.jsonSchema.impl.JsonSchemaVersion version,
                      @NotNull final String name,
                      @NotNull final String file,
                      @NotNull final List<? extends PairProcessor<Project, VirtualFile>> patterns) {
      myProject = project;
      myVersion = version;
      myName = name;
      myFile = file;
      myPatterns = patterns;
    }

    @Override
    public JsonSchemaVersion getSchemaVersion() {
      return myVersion;
    }

    @Nullable
    @Override
    public VirtualFile getSchemaFile() {
      if (myVirtualFile != null && myVirtualFile.isValid()) return myVirtualFile;
      String path = myFile;

      if (fit.jetbrains.jsonSchema.remote.JsonFileResolver.isAbsoluteUrl(path)) {
        myVirtualFile = fit.jetbrains.jsonSchema.remote.JsonFileResolver.urlToFile(path);
      }
      else {
        final LocalFileSystem lfs = LocalFileSystem.getInstance();
        myVirtualFile = lfs.findFileByPath(myFile);
        if (myVirtualFile == null) {
          myVirtualFile = lfs.refreshAndFindFileByPath(myFile);
        }
      }
      return myVirtualFile;
    }

    @NotNull
    @Override
    public SchemaType getSchemaType() {
      return SchemaType.userSchema;
    }

    @NotNull
    @Override
    public String getName() {
      return myName;
    }

    @Override
    public boolean isAvailable(@NotNull VirtualFile file) {
      //noinspection SimplifiableIfStatement
      if (myPatterns.isEmpty() || file.isDirectory() || !file.isValid()) return false;
      return myPatterns.stream().anyMatch(processor -> processor.process(myProject, file));
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;

      MyProvider provider = (MyProvider)o;

      if (!myName.equals(provider.myName)) return false;
      return FileUtil.pathsEqual(myFile, provider.myFile);
    }

    @Override
    public int hashCode() {
      int result = myName.hashCode();
      result = 31 * result + FileUtil.pathHashCode(myFile);
      return result;
    }

    @Nullable
    @Override
    public String getRemoteSource() {
      return fit.jetbrains.jsonSchema.remote.JsonFileResolver.isHttpPath(myFile) ? myFile : null;
    }
  }
}
