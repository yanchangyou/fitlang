// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.jsonpath;

import com.intellij.codeInsight.editorActions.SimpleTokenSetQuoteHandler;
import fit.intellij.jsonpath.psi.JsonPathTypes;

public final class JsonPathQuoteHandler extends SimpleTokenSetQuoteHandler {
  public JsonPathQuoteHandler() {
    super(fit.intellij.jsonpath.psi.JsonPathTypes.SINGLE_QUOTED_STRING, JsonPathTypes.DOUBLE_QUOTED_STRING);
  }
}
