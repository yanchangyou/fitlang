// Copyright 2000-2021 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.jsonLines

import com.intellij.icons.AllIcons
import com.intellij.openapi.util.NlsContexts
import com.intellij.openapi.util.NlsSafe
import fit.intellij.json.JsonBundle
import fit.intellij.json.JsonFileType
import org.jetbrains.annotations.NonNls
import javax.swing.Icon

class JsonLinesFileType private constructor() : JsonFileType(JsonLinesLanguage) {
    override fun getName(): @NonNls String = "Fit-lines"

    override fun getDescription(): @NlsContexts.Label String = JsonBundle.message("filetype.json_lines.description")

    override fun getDefaultExtension(): @NlsSafe String = "fitl"

    override fun getIcon(): Icon = AllIcons.FileTypes.Json

    companion object {
        @JvmField
        val INSTANCE = JsonLinesFileType()
    }
}