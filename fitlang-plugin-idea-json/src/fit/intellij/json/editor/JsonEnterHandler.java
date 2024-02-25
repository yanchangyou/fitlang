// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.editor;

import com.intellij.codeInsight.editorActions.EnterHandler;
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegateAdapter;
import com.intellij.lang.Language;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.ObjectUtils;
import fit.intellij.json.JsonElementTypes;
import fit.intellij.json.JsonLanguage;
import fit.intellij.json.psi.JsonProperty;
import org.jetbrains.annotations.NotNull;

public class JsonEnterHandler extends EnterHandlerDelegateAdapter {
  @Override
  public Result preprocessEnter(@NotNull PsiFile file,
                                @NotNull Editor editor,
                                @NotNull Ref<Integer> caretOffsetRef,
                                @NotNull Ref<Integer> caretAdvanceRef,
                                @NotNull DataContext dataContext,
                                EditorActionHandler originalHandler) {
    if (!JsonEditorOptions.getInstance().COMMA_ON_ENTER) {
      return Result.Continue;
    }

    Language language = EnterHandler.getLanguage(dataContext);
    if (!(language instanceof fit.intellij.json.JsonLanguage)) {
      return Result.Continue;
    }

    int caretOffset = caretOffsetRef.get().intValue();
    PsiElement psiAtOffset = file.findElementAt(caretOffset);

    if (psiAtOffset == null) {
      return Result.Continue;
    }

    if (psiAtOffset instanceof LeafPsiElement && handleComma(caretOffsetRef, psiAtOffset, editor)) {
      return Result.Continue;
    }

    fit.intellij.json.psi.JsonValue literal = ObjectUtils.tryCast(psiAtOffset.getParent(), fit.intellij.json.psi.JsonValue.class);
    if (literal != null && (!(literal instanceof fit.intellij.json.psi.JsonStringLiteral) || !((JsonLanguage)language).hasPermissiveStrings())) {
      handleJsonValue(literal, editor, caretOffsetRef);
    }

    return Result.Continue;
  }

  private static boolean handleComma(@NotNull Ref<Integer> caretOffsetRef, @NotNull PsiElement psiAtOffset, @NotNull Editor editor) {
    PsiElement nextSibling = psiAtOffset;
    boolean hasNewlineBefore = false;
    while (nextSibling instanceof PsiWhiteSpace) {
      hasNewlineBefore = nextSibling.getText().contains("\n");
      nextSibling = nextSibling.getNextSibling();
    }

    LeafPsiElement leafPsiElement = ObjectUtils.tryCast(nextSibling, LeafPsiElement.class);
    IElementType elementType = leafPsiElement == null ? null : leafPsiElement.getElementType();
    if (elementType == fit.intellij.json.JsonElementTypes.COMMA || elementType == fit.intellij.json.JsonElementTypes.R_CURLY) {
      PsiElement prevSibling = nextSibling.getPrevSibling();
      while (prevSibling instanceof PsiWhiteSpace) {
        prevSibling = prevSibling.getPrevSibling();
      }

      if (prevSibling instanceof fit.intellij.json.psi.JsonProperty && ((fit.intellij.json.psi.JsonProperty)prevSibling).getValue() != null) {
        int offset = elementType == fit.intellij.json.JsonElementTypes.COMMA ? nextSibling.getTextRange().getEndOffset() : prevSibling.getTextRange().getEndOffset();
        if (offset < editor.getDocument().getTextLength()) {
          if (elementType == fit.intellij.json.JsonElementTypes.R_CURLY && hasNewlineBefore) {
            editor.getDocument().insertString(offset, ",");
            offset++;
          }
          caretOffsetRef.set(offset);
        }
        return true;
      }
      return false;
    }

    if (nextSibling instanceof fit.intellij.json.psi.JsonProperty) {
      PsiElement prevSibling = nextSibling.getPrevSibling();
      while (prevSibling instanceof PsiWhiteSpace || prevSibling instanceof PsiErrorElement) {
        prevSibling = prevSibling.getPrevSibling();
      }

      if (prevSibling instanceof fit.intellij.json.psi.JsonProperty) {
        int offset = prevSibling.getTextRange().getEndOffset();
        if (offset < editor.getDocument().getTextLength()) {
          editor.getDocument().insertString(offset, ",");
          caretOffsetRef.set(offset + 1);
        }
        return true;
      }
    }

    return false;
  }

  private static void handleJsonValue(@NotNull fit.intellij.json.psi.JsonValue literal, @NotNull Editor editor, @NotNull Ref<Integer> caretOffsetRef) {
    PsiElement parent = literal.getParent();
    if (!(parent instanceof fit.intellij.json.psi.JsonProperty) || ((fit.intellij.json.psi.JsonProperty)parent).getValue() != literal) {
      return;
    }

    PsiElement nextSibling = parent.getNextSibling();
    while (nextSibling instanceof PsiWhiteSpace || nextSibling instanceof PsiErrorElement) {
      nextSibling = nextSibling.getNextSibling();
    }

    int offset = literal.getTextRange().getEndOffset();

    if (literal instanceof fit.intellij.json.psi.JsonObject || literal instanceof fit.intellij.json.psi.JsonArray) {
      if (nextSibling instanceof LeafPsiElement && ((LeafPsiElement)nextSibling).getElementType() == fit.intellij.json.JsonElementTypes.COMMA
        || !(nextSibling instanceof JsonProperty)) {
        return;
      }
      Document document = editor.getDocument();
      if (offset < document.getTextLength()) {
        document.insertString(offset, ",");
      }
      return;
    }

    if (nextSibling instanceof LeafPsiElement && ((LeafPsiElement)nextSibling).getElementType() == JsonElementTypes.COMMA) {
      offset = nextSibling.getTextRange().getEndOffset();
    }
    else {
      Document document = editor.getDocument();
      if (offset < document.getTextLength()) {
        document.insertString(offset, ",");
      }
      offset++;
    }

    if (offset < editor.getDocument().getTextLength()) {
      caretOffsetRef.set(offset);
    }
  }
}
