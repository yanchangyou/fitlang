// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.formatter;

import com.intellij.openapi.editor.DefaultLineWrapPositionStrategy;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import fit.intellij.json.JsonElementTypes;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class JsonLineWrapPositionStrategy extends DefaultLineWrapPositionStrategy {
  @Override
  public int calculateWrapPosition(@NotNull Document document,
                                   @Nullable Project project,
                                   int startOffset,
                                   int endOffset,
                                   int maxPreferredOffset,
                                   boolean allowToBeyondMaxPreferredOffset,
                                   boolean isSoftWrap) {
    if (isSoftWrap) {
      return super.calculateWrapPosition(document, project, startOffset, endOffset, maxPreferredOffset, allowToBeyondMaxPreferredOffset,
                             true);
    }
    if (project == null) return -1;
    final int wrapPosition = getMinWrapPosition(document, project, maxPreferredOffset);
    if (wrapPosition == SKIP_WRAPPING) return -1;
    int minWrapPosition = Math.max(startOffset, wrapPosition);
    return super
      .calculateWrapPosition(document, project, minWrapPosition, endOffset, maxPreferredOffset, allowToBeyondMaxPreferredOffset, isSoftWrap);
  }

  private static final int SKIP_WRAPPING = -2;
  private static int getMinWrapPosition(@NotNull Document document, @NotNull Project project, int offset) {
    PsiDocumentManager manager = PsiDocumentManager.getInstance(project);
    if (manager.isUncommited(document)) manager.commitDocument(document);
    PsiFile psiFile = manager.getPsiFile(document);
    if (psiFile != null) {
      PsiElement currElement = psiFile.findElementAt(offset);
      final IElementType elementType = PsiUtilCore.getElementType(currElement);
      if (elementType == fit.intellij.json.JsonElementTypes.DOUBLE_QUOTED_STRING
          || elementType == fit.intellij.json.JsonElementTypes.SINGLE_QUOTED_STRING
          || elementType == fit.intellij.json.JsonElementTypes.LITERAL
          || elementType == fit.intellij.json.JsonElementTypes.BOOLEAN_LITERAL
          || elementType == fit.intellij.json.JsonElementTypes.TRUE
          || elementType == fit.intellij.json.JsonElementTypes.FALSE
          || elementType == fit.intellij.json.JsonElementTypes.IDENTIFIER
          || elementType == fit.intellij.json.JsonElementTypes.NULL_LITERAL
          || elementType == fit.intellij.json.JsonElementTypes.NUMBER_LITERAL) {
        return currElement.getTextRange().getEndOffset();
      }
      if (elementType == fit.intellij.json.JsonElementTypes.COLON) {
        return SKIP_WRAPPING;
      }
      if (currElement != null) {
        if (currElement instanceof PsiComment ||
            PsiUtilCore.getElementType(PsiTreeUtil.skipWhitespacesForward(currElement)) == JsonElementTypes.COMMA) {
          return SKIP_WRAPPING;
        }
      }
    }
    return -1;
  }
}
