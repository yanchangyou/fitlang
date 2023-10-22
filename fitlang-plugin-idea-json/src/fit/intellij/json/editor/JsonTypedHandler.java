// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.editor;

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.editorActions.TypedHandlerDelegate;
import com.intellij.codeInsight.editorActions.smartEnter.SmartEnterProcessor;
import fit.intellij.json.JsonDialectUtil;
import fit.intellij.json.JsonElementTypes;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;

public class JsonTypedHandler extends TypedHandlerDelegate {

  private boolean myWhitespaceAdded;

  @NotNull
  @Override
  public Result charTyped(char c, @NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (file instanceof fit.intellij.json.psi.JsonFile) {
      processPairedBracesComma(c, editor, file);
      addWhiteSpaceAfterColonIfNeeded(c, editor, file);
      removeRedundantWhitespaceIfAfterColon(c, editor, file);
      handleMoveOutsideQuotes(c, editor, file);
    }
    return Result.CONTINUE;
  }

  private static void handleMoveOutsideQuotes(char c, Editor editor, PsiFile file) {
    fit.intellij.json.editor.JsonEditorOptions options = fit.intellij.json.editor.JsonEditorOptions.getInstance();
    if (c == ':' && options.COLON_MOVE_OUTSIDE_QUOTES || c == ',' && options.COMMA_MOVE_OUTSIDE_QUOTES) {
      int offset = editor.getCaretModel().getOffset();
      CharSequence sequence = editor.getDocument().getCharsSequence();
      int length = sequence.length();
      if (offset >= length || offset < 0) return;
      char charAtOffset = sequence.charAt(offset);
      if (charAtOffset != '"') return;
      if (offset + 1 < length && sequence.charAt(offset + 1) == c) return;
      final PsiElement element = file.findElementAt(offset);
      if (element == null) return;
      if (!validatePositionToMoveOutOfQuotes(c, element)) return;
      PsiDocumentManager.getInstance(file.getProject()).commitDocument(editor.getDocument());
      editor.getDocument().deleteString(offset - 1, offset);
      editor.getDocument().insertString(offset, String.valueOf(c));
      CharSequence newSequence = editor.getDocument().getCharsSequence();
      int nextOffset = offset + 1;
      if (c == ':' && options.AUTO_WHITESPACE_AFTER_COLON) {
        char nextChar = nextOffset >= newSequence.length() ? 'a' : newSequence.charAt(nextOffset);
        if (!Character.isWhitespace(nextChar) || nextChar == '\n') {
          editor.getDocument().insertString(nextOffset, " ");
          nextOffset++;
        }
      }
      editor.getCaretModel().moveToOffset(nextOffset);
    }
  }

  private static boolean validatePositionToMoveOutOfQuotes(char c, PsiElement element) {
    // comma can be after the element, but only the comma
    if (PsiUtilCore.getElementType(element) == JsonElementTypes.R_CURLY) {
      return c == ',' && element.getPrevSibling() instanceof fit.intellij.json.psi.JsonProperty;
    }
    if (PsiUtilCore.getElementType(element) == JsonElementTypes.R_BRACKET) {
      return c == ',' && element.getPrevSibling() instanceof fit.intellij.json.psi.JsonStringLiteral;
    }

    // we can have a whitespace in the position, but again - only for the comma
    PsiElement parent = element.getParent();
    if (element instanceof PsiWhiteSpace && c == ',') {
      PsiElement sibling = element.getPrevSibling();
      return sibling instanceof fit.intellij.json.psi.JsonProperty || sibling instanceof fit.intellij.json.psi.JsonStringLiteral;
    }

    // the most ordinary case - literal property key or value
    PsiElement grandParent = parent instanceof fit.intellij.json.psi.JsonStringLiteral ? parent.getParent() : null;
    return grandParent instanceof fit.intellij.json.psi.JsonProperty
           && (c != ':' || ((fit.intellij.json.psi.JsonProperty)grandParent).getNameElement() == parent)
           && (c != ',' || ((fit.intellij.json.psi.JsonProperty)grandParent).getValue() == parent);
  }

  private void removeRedundantWhitespaceIfAfterColon(char c, Editor editor, PsiFile file) {
    if (!myWhitespaceAdded || c != ' ' || !fit.intellij.json.editor.JsonEditorOptions.getInstance().AUTO_WHITESPACE_AFTER_COLON) {
      if (c != ':') {
        myWhitespaceAdded = false;
      }
      return;
    }
    int offset = editor.getCaretModel().getOffset();
    PsiDocumentManager.getInstance(file.getProject()).commitDocument(editor.getDocument());
    final PsiElement element = file.findElementAt(offset);
    if (element instanceof PsiWhiteSpace) {
      editor.getDocument().deleteString(offset - 1, offset);
    }
    myWhitespaceAdded = false;
  }

  @NotNull
  @Override
  public Result beforeCharTyped(char c,
                                @NotNull Project project,
                                @NotNull Editor editor,
                                @NotNull PsiFile file,
                                @NotNull FileType fileType) {
    if (file instanceof fit.intellij.json.psi.JsonFile) {
      addPropertyNameQuotesIfNeeded(c, editor, file);
    }
    return Result.CONTINUE;
  }

  private void addWhiteSpaceAfterColonIfNeeded(char c,
                                               @NotNull Editor editor,
                                               @NotNull PsiFile file) {
    if (c != ':' || !fit.intellij.json.editor.JsonEditorOptions.getInstance().AUTO_WHITESPACE_AFTER_COLON) {
      if (c != ' ') {
        myWhitespaceAdded = false;
      }
      return;
    }
    int offset = editor.getCaretModel().getOffset();
    PsiDocumentManager.getInstance(file.getProject()).commitDocument(editor.getDocument());
    PsiElement element = PsiTreeUtil.getParentOfType(PsiTreeUtil.skipWhitespacesBackward(file.findElementAt(offset)), fit.intellij.json.psi.JsonProperty.class, false);
    if (element == null) {
      myWhitespaceAdded = false;
      return;
    }
    final ASTNode[] children = element.getNode().getChildren(TokenSet.create(JsonElementTypes.COLON));
    if (children.length == 0) {
      myWhitespaceAdded = false;
      return;
    }
    final ASTNode colon = children[0];
    final ASTNode next = colon.getTreeNext();
    final String text = next.getText();
    if (text.length() == 0 || !StringUtil.isEmptyOrSpaces(text) || StringUtil.isLineBreak(text.charAt(0))) {
      final int insOffset = colon.getStartOffset() + 1;
      editor.getDocument().insertString(insOffset, " ");
      editor.getCaretModel().moveToOffset(insOffset + 1);
      myWhitespaceAdded = true;
    }
    else {
      myWhitespaceAdded = false;
    }
  }

  private static void addPropertyNameQuotesIfNeeded(char c,
                                                    @NotNull Editor editor,
                                                    @NotNull PsiFile file) {
    if (c != ':' || !JsonDialectUtil.isStandardJson(file) || !fit.intellij.json.editor.JsonEditorOptions.getInstance().AUTO_QUOTE_PROP_NAME) return;
    int offset = editor.getCaretModel().getOffset();
    PsiElement element = PsiTreeUtil.skipWhitespacesBackward(file.findElementAt(offset));
    if (!(element instanceof fit.intellij.json.psi.JsonProperty)) return;
    final fit.intellij.json.psi.JsonValue nameElement = ((fit.intellij.json.psi.JsonProperty)element).getNameElement();
    if (nameElement instanceof fit.intellij.json.psi.JsonReferenceExpression) {
      ((fit.intellij.json.psi.JsonProperty)element).setName(nameElement.getText());
      PsiDocumentManager.getInstance(file.getProject()).doPostponedOperationsAndUnblockDocument(editor.getDocument());
    }
  }

  public static void processPairedBracesComma(char c,
                                              @NotNull Editor editor,
                                              @NotNull PsiFile file) {
    if (!JsonEditorOptions.getInstance().COMMA_ON_MATCHING_BRACES) return;
    if (c != '[' && c != '{' && c != '"' && c != '\'') return;
    SmartEnterProcessor.commitDocument(editor);
    int offset = editor.getCaretModel().getOffset();
    PsiElement element = file.findElementAt(offset);
    if (element == null) return;
    PsiElement parent = element.getParent();
    CodeInsightSettings codeInsightSettings = CodeInsightSettings.getInstance();
    if ((c == '[' && parent instanceof fit.intellij.json.psi.JsonArray
         || c == '{' && parent instanceof fit.intellij.json.psi.JsonObject) && codeInsightSettings.AUTOINSERT_PAIR_BRACKET
        || (c == '"' || c == '\'') && parent instanceof fit.intellij.json.psi.JsonStringLiteral && codeInsightSettings.AUTOINSERT_PAIR_QUOTE) {
      if (shouldAddCommaInParentContainer((fit.intellij.json.psi.JsonValue)parent)) {
        editor.getDocument().insertString(parent.getTextRange().getEndOffset(), ",");
      }
    }
  }

  private static boolean shouldAddCommaInParentContainer(@NotNull fit.intellij.json.psi.JsonValue item) {
    PsiElement parent = item.getParent();
    if (parent instanceof fit.intellij.json.psi.JsonArray || parent instanceof fit.intellij.json.psi.JsonProperty) {
      PsiElement nextElement = PsiTreeUtil.skipWhitespacesForward(parent instanceof fit.intellij.json.psi.JsonProperty ? parent : item);
      if (nextElement instanceof PsiErrorElement) {
        PsiElement forward = PsiTreeUtil.skipWhitespacesForward(nextElement);
        return parent instanceof fit.intellij.json.psi.JsonProperty ? forward instanceof fit.intellij.json.psi.JsonProperty : forward instanceof JsonValue;
      }
    }
    return false;
  }
}
