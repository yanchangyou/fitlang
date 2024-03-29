// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.json5.highlighting;

import com.intellij.lexer.Lexer;
import fit.intellij.json.highlighting.JsonSyntaxHighlighterFactory;
import fit.intellij.json.json5.Json5Lexer;
import org.jetbrains.annotations.NotNull;

public class Json5SyntaxHighlightingFactory extends JsonSyntaxHighlighterFactory {
  @NotNull
  @Override
  protected Lexer getLexer() {
    return new Json5Lexer();
  }

  @Override
  protected boolean isCanEscapeEol() {
    return true;
  }
}
