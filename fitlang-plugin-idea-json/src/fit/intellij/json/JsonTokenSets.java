// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
package fit.intellij.json;

import com.intellij.psi.tree.TokenSet;

public final class JsonTokenSets {
  public static final TokenSet STRING_LITERALS = TokenSet.create(JsonElementTypes.SINGLE_QUOTED_STRING, JsonElementTypes.DOUBLE_QUOTED_STRING);

  public static final TokenSet JSON_CONTAINERS = TokenSet.create(JsonElementTypes.OBJECT, JsonElementTypes.ARRAY);
  public static final TokenSet JSON_KEYWORDS = TokenSet.create(JsonElementTypes.TRUE, JsonElementTypes.FALSE, JsonElementTypes.NULL);
  public static final TokenSet JSON_LITERALS = TokenSet.create(JsonElementTypes.STRING_LITERAL, JsonElementTypes.NUMBER_LITERAL, JsonElementTypes.NULL_LITERAL, JsonElementTypes.TRUE, JsonElementTypes.FALSE);
  public static final TokenSet JSON_COMMENTARIES = TokenSet.create(JsonElementTypes.BLOCK_COMMENT, JsonElementTypes.LINE_COMMENT);
}