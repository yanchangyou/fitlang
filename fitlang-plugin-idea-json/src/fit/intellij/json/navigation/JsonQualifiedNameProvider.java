package fit.intellij.json.navigation;

import com.intellij.ide.actions.QualifiedNameProvider;
import fit.intellij.json.JsonUtil;
import fit.intellij.json.psi.JsonElement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonProperty;
import fit.jetbrains.jsonSchema.JsonPointerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author Mikhail Golubev
 */
public class JsonQualifiedNameProvider implements QualifiedNameProvider {
  @Nullable
  @Override
  public PsiElement adjustElementToCopy(@NotNull PsiElement element) {
    return null;
  }

  @Nullable
  @Override
  public String getQualifiedName(@NotNull PsiElement element) {
    return generateQualifiedName(element, fit.intellij.json.navigation.JsonQualifiedNameKind.Qualified);
  }

  public static String generateQualifiedName(PsiElement element, fit.intellij.json.navigation.JsonQualifiedNameKind qualifiedNameKind) {
    if (!(element instanceof JsonElement)) {
      return null;
    }
    JsonElement parentProperty = PsiTreeUtil.getNonStrictParentOfType(element, fit.intellij.json.psi.JsonProperty.class, fit.intellij.json.psi.JsonArray.class);
    StringBuilder builder = new StringBuilder();
    while (parentProperty != null) {
      if (parentProperty instanceof fit.intellij.json.psi.JsonProperty jsonProperty) {
        String name = jsonProperty.getName();
        if (qualifiedNameKind == fit.intellij.json.navigation.JsonQualifiedNameKind.JsonPointer) {
          name = JsonPointerUtil.escapeForJsonPointer(name);
        }
        builder.insert(0, name);
        builder.insert(0, qualifiedNameKind == fit.intellij.json.navigation.JsonQualifiedNameKind.JsonPointer ? "/" : ".");
      }
      else {
        int index = JsonUtil.getArrayIndexOfItem(element instanceof fit.intellij.json.psi.JsonProperty ? element.getParent() : element);
        if (index == -1) return null;
        builder.insert(0, qualifiedNameKind == JsonQualifiedNameKind.JsonPointer ? ("/" + index) : ("[" + index + "]"));
      }
      element = parentProperty;
      parentProperty = PsiTreeUtil.getParentOfType(parentProperty, JsonProperty.class, JsonArray.class);
    }

    if (builder.length() == 0) return null;

    // if the first operation is array indexing, we insert the 'root' element $
    if (builder.charAt(0) == '[') {
      builder.insert(0, "$");
    }

    return StringUtil.trimStart(builder.toString(), ".");
  }

  @Override
  public PsiElement qualifiedNameToElement(@NotNull String fqn, @NotNull Project project) {
    return null;
  }
}
