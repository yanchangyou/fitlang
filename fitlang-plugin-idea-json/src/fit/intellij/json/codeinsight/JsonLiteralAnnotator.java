// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.codeinsight;

import fit.intellij.json.highlighting.JsonSyntaxHighlighterFactory;
import fit.intellij.json.psi.JsonNumberLiteral;
import fit.intellij.json.psi.JsonReferenceExpression;
import fit.intellij.json.psi.JsonStringLiteral;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.psi.JsonPsiUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author Mikhail Golubev
 */
public class JsonLiteralAnnotator implements Annotator {

  private static class Holder {
    private static final boolean DEBUG = ApplicationManager.getApplication().isUnitTestMode();
  }

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    List<fit.intellij.json.codeinsight.JsonLiteralChecker> extensions = fit.intellij.json.codeinsight.JsonLiteralChecker.EP_NAME.getExtensionList();
    if (element instanceof JsonReferenceExpression) {
      highlightPropertyKey(element, holder);
    }
    else if (element instanceof JsonStringLiteral) {
      final JsonStringLiteral stringLiteral = (JsonStringLiteral)element;
      final int elementOffset = element.getTextOffset();
      highlightPropertyKey(element, holder);
      final String text = fit.intellij.json.psi.JsonPsiUtil.getElementTextWithoutHostEscaping(element);
      final int length = text.length();

      // Check that string literal is closed properly
      if (length <= 1 || text.charAt(0) != text.charAt(length - 1) || fit.intellij.json.psi.JsonPsiUtil.isEscapedChar(text, length - 1)) {
        holder.newAnnotation(HighlightSeverity.ERROR, fit.intellij.json.JsonBundle.message("syntax.error.missing.closing.quote")).create();
      }

      // Check escapes
      final List<Pair<TextRange, String>> fragments = stringLiteral.getTextFragments();
      for (Pair<TextRange, String> fragment: fragments) {
        for (fit.intellij.json.codeinsight.JsonLiteralChecker checker: extensions) {
          if (!checker.isApplicable(element)) continue;
          Pair<TextRange, String> error = checker.getErrorForStringFragment(fragment, stringLiteral);
          if (error != null) {
            holder.newAnnotation(HighlightSeverity.ERROR, error.second).range(error.getFirst().shiftRight(elementOffset)).create();
          }
        }
      }
    }
    else if (element instanceof JsonNumberLiteral) {
      String text = null;
      for (JsonLiteralChecker checker: extensions) {
        if (!checker.isApplicable(element)) continue;
        if (text == null) {
          text = fit.intellij.json.psi.JsonPsiUtil.getElementTextWithoutHostEscaping(element);
        }
        String error = checker.getErrorForNumericLiteral(text);
        if (error != null) {
          holder.newAnnotation(HighlightSeverity.ERROR, error).create();
        }
      }
    }
  }

  private static void highlightPropertyKey(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (JsonPsiUtil.isPropertyKey(element)) {
//      if (Holder.DEBUG) {
//        holder.newAnnotation(HighlightSeverity.INFORMATION, JsonBundle.message("annotation.property.key")).textAttributes(JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY).create();
//      }
//      else {
      if (JsonPsiUtil.isKeyword(element)) {
        holder.newSilentAnnotation(HighlightSeverity.INFORMATION).textAttributes(JsonSyntaxHighlighterFactory.MY_KEYWORD).create();
      } else {
        holder.newSilentAnnotation(HighlightSeverity.INFORMATION).textAttributes(JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY).create();
      }
//      }
    }
  }
}
