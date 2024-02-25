// Copyright 2000-2021 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.jsonLines

import fit.intellij.json.JsonParserDefinition
import fit.intellij.json.psi.impl.JsonFileImpl
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IFileElementType

class JsonLinesParserDefinition: JsonParserDefinition() {
  override fun createFile(fileViewProvider: FileViewProvider): PsiFile {
    return JsonFileImpl(fileViewProvider, JsonLinesLanguage)
  }

  override fun getFileNodeType(): IFileElementType = FILE

  companion object {
    val FILE = IFileElementType(JsonLinesLanguage)
  }
}