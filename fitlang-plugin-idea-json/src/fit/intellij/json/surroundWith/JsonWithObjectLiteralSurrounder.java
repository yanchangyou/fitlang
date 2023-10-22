package fit.intellij.json.surroundWith;

import fit.intellij.json.JsonBundle;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import fit.intellij.json.psi.JsonElement;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * This surrounder ported from JavaScript allows to wrap single JSON value or several consecutive JSON properties
 * in object literal.
 * <p/>
 * Examples:
 * <ol>
 * <li>{@code [42]} converts to {@code [{"property": 42}]}</li>
 * <li><pre>
 * {
 *    "foo": 42,
 *    "bar": false
 * }
 * </pre> converts to <pre>
 * {
 *    "property": {
 *      "foo": 42,
 *      "bar": false
 *    }
 * }
 * </pre></li>
 * </ol>
 *
 * @author Mikhail Golubev
 */
public class JsonWithObjectLiteralSurrounder extends JsonSurrounderBase {
  @Override
  public String getTemplateDescription() {
    return JsonBundle.message("surround.with.object.literal.desc");
  }

  @Override
  public boolean isApplicable(@NotNull PsiElement[] elements) {
    return !fit.intellij.json.psi.JsonPsiUtil.isPropertyKey(elements[0]) && (elements[0] instanceof fit.intellij.json.psi.JsonProperty || elements.length == 1);
  }

  @Nullable
  @Override
  public TextRange surroundElements(@NotNull Project project,
                                    @NotNull Editor editor,
                                    @NotNull PsiElement[] elements) throws IncorrectOperationException {

    if (!isApplicable(elements)) {
      return null;
    }

    final fit.intellij.json.psi.JsonElementGenerator generator = new fit.intellij.json.psi.JsonElementGenerator(project);

    final PsiElement firstElement = elements[0];
    final JsonElement newNameElement;
    if (firstElement instanceof JsonValue) {
      assert elements.length == 1 : "Only single JSON value can be wrapped in object literal";
      fit.intellij.json.psi.JsonObject replacement = generator.createValue(createReplacementText(firstElement.getText()));
      replacement = (fit.intellij.json.psi.JsonObject)firstElement.replace(replacement);
      newNameElement = replacement.getPropertyList().get(0).getNameElement();
    }
    else {
      assert firstElement instanceof fit.intellij.json.psi.JsonProperty;
      final String propertiesText = getTextAndRemoveMisc(firstElement, elements[elements.length - 1]);
      final fit.intellij.json.psi.JsonObject tempJsonObject = generator.createValue(createReplacementText("{\n" + propertiesText) + "\n}");
      fit.intellij.json.psi.JsonProperty replacement = tempJsonObject.getPropertyList().get(0);
      replacement = (fit.intellij.json.psi.JsonProperty)firstElement.replace(replacement);
      newNameElement = replacement.getNameElement();
    }
    final TextRange rangeWithQuotes = newNameElement.getTextRange();
    return new TextRange(rangeWithQuotes.getStartOffset() + 1, rangeWithQuotes.getEndOffset() - 1);
  }

  @NotNull
  @Override
  protected String createReplacementText(@NotNull String textInRange) {
    return "{\n\"property\": " + textInRange + "\n}";
  }
}
