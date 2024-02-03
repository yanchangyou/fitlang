// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.json5;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;
import fit.intellij.json.JsonParserDefinition;
import fit.intellij.json.psi.impl.JsonFileImpl;
import org.jetbrains.annotations.NotNull;

public class Json5ParserDefinition extends JsonParserDefinition {
  public static final IFileElementType FILE = new IFileElementType(fit.intellij.json.json5.Json5Language.INSTANCE);

  @NotNull
  @Override
  public Lexer createLexer(Project project) {
    return new Json5Lexer();
  }

  @Override
  public @NotNull PsiFile createFile(@NotNull FileViewProvider fileViewProvider) {
    return new JsonFileImpl(fileViewProvider, Json5Language.INSTANCE);
  }

  @Override
  public @NotNull IFileElementType getFileNodeType() {
    return FILE;
  }
}
