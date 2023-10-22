package fit.intellij.json.editor.smartEnter;

import fit.intellij.json.JsonDialectUtil;
import com.intellij.lang.SmartEnterProcessorWithFixers;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static fit.intellij.json.JsonElementTypes.COLON;
import static fit.intellij.json.JsonElementTypes.COMMA;

/**
 * This processor allows
 * <ul>
 * <li>Insert colon after key inside object property</li>
 * <li>Insert comma after array element or object property</li>
 * </ul>
 *
 * @author Mikhail Golubev
 */
public class JsonSmartEnterProcessor extends SmartEnterProcessorWithFixers {
  public static final Logger LOG = Logger.getInstance(JsonSmartEnterProcessor.class);

  private boolean myShouldAddNewline = false;

  public JsonSmartEnterProcessor() {
    addFixers(new JsonObjectPropertyFixer(), new JsonArrayElementFixer());
    addEnterProcessors(new JsonEnterProcessor());
  }

  @Override
  protected void collectAdditionalElements(@NotNull PsiElement element, @NotNull List<PsiElement> result) {
    // include all parents as well
    PsiElement parent = element.getParent();
    while (parent != null && !(parent instanceof fit.intellij.json.psi.JsonFile)) {
      result.add(parent);
      parent = parent.getParent();
    }
  }

  private static boolean terminatedOnCurrentLine(@NotNull Editor editor, @NotNull PsiElement element) {
    final Document document = editor.getDocument();
    final int caretOffset = editor.getCaretModel().getCurrentCaret().getOffset();
    final int elementEndOffset = element.getTextRange().getEndOffset();
    if (document.getLineNumber(elementEndOffset) != document.getLineNumber(caretOffset)) {
      return false;
    }
    // Skip empty PsiError elements if comma is missing
    PsiElement nextLeaf = PsiTreeUtil.nextLeaf(element, true);
    return nextLeaf == null || (nextLeaf instanceof PsiWhiteSpace && nextLeaf.getText().contains("\n"));
  }

  private static boolean isFollowedByTerminal(@NotNull PsiElement element, IElementType type) {
    final PsiElement nextLeaf = PsiTreeUtil.nextVisibleLeaf(element);
    return nextLeaf != null && nextLeaf.getNode().getElementType() == type;
  }

  private static class JsonArrayElementFixer extends Fixer<JsonSmartEnterProcessor> {
    @Override
    public void apply(@NotNull Editor editor, @NotNull JsonSmartEnterProcessor processor, @NotNull PsiElement element)
      throws IncorrectOperationException {
      if (element instanceof fit.intellij.json.psi.JsonValue && element.getParent() instanceof fit.intellij.json.psi.JsonArray) {
        final fit.intellij.json.psi.JsonValue arrayElement = (fit.intellij.json.psi.JsonValue)element;
        if (terminatedOnCurrentLine(editor, arrayElement) && !isFollowedByTerminal(element, COMMA)) {
          editor.getDocument().insertString(arrayElement.getTextRange().getEndOffset(), ",");
          processor.myShouldAddNewline = true;
        }
      }
    }
  }

  private static class JsonObjectPropertyFixer extends Fixer<JsonSmartEnterProcessor> {
    @Override
    public void apply(@NotNull Editor editor, @NotNull JsonSmartEnterProcessor processor, @NotNull PsiElement element)
      throws IncorrectOperationException {
      if (element instanceof fit.intellij.json.psi.JsonProperty) {
        final fit.intellij.json.psi.JsonValue propertyValue = ((fit.intellij.json.psi.JsonProperty)element).getValue();
        if (propertyValue != null) {
          if (terminatedOnCurrentLine(editor, propertyValue) && !isFollowedByTerminal(propertyValue, COMMA)) {
            editor.getDocument().insertString(propertyValue.getTextRange().getEndOffset(), ",");
            processor.myShouldAddNewline = true;
          }
        }
        else {
          final JsonValue propertyKey = ((fit.intellij.json.psi.JsonProperty)element).getNameElement();
          TextRange keyRange = propertyKey.getTextRange();
          final int keyStartOffset = keyRange.getStartOffset();
          int keyEndOffset = keyRange.getEndOffset();
          //processor.myFirstErrorOffset = keyEndOffset;
          if (terminatedOnCurrentLine(editor, propertyKey) && !isFollowedByTerminal(propertyKey, COLON)) {
            boolean shouldQuoteKey = propertyKey instanceof fit.intellij.json.psi.JsonReferenceExpression && JsonDialectUtil.isStandardJson(propertyKey);
            if (shouldQuoteKey) {
              editor.getDocument().insertString(keyStartOffset, "\"");
              keyEndOffset++;
              editor.getDocument().insertString(keyEndOffset, "\"");
              keyEndOffset++;
            }
            processor.myFirstErrorOffset = keyEndOffset + 2;
            editor.getDocument().insertString(keyEndOffset, ": ");
          }
        }
      }
    }
  }

  private class JsonEnterProcessor extends FixEnterProcessor {
    @Override
    public boolean doEnter(PsiElement atCaret, PsiFile file, @NotNull Editor editor, boolean modified) {
      if (myShouldAddNewline) {
        try {
          plainEnter(editor);
        }
        finally {
          myShouldAddNewline = false;
        }
      }
      return true;
    }
  }
}
