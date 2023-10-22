package fit.intellij.json.structureView;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonFile;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author Mikhail Golubev
 */
public class JsonStructureViewElement implements StructureViewTreeElement {
  private final fit.intellij.json.psi.JsonElement myElement;

  public JsonStructureViewElement(@NotNull fit.intellij.json.psi.JsonElement element) {
    assert PsiTreeUtil.instanceOf(element, fit.intellij.json.psi.JsonFile.class, fit.intellij.json.psi.JsonProperty.class, fit.intellij.json.psi.JsonObject.class, fit.intellij.json.psi.JsonArray.class);
    myElement = element;
  }

  @Override
  public fit.intellij.json.psi.JsonElement getValue() {
    return myElement;
  }

  @Override
  public void navigate(boolean requestFocus) {
    myElement.navigate(requestFocus);
  }

  @Override
  public boolean canNavigate() {
    return myElement.canNavigate();
  }

  @Override
  public boolean canNavigateToSource() {
    return myElement.canNavigateToSource();
  }

  @NotNull
  @Override
  public ItemPresentation getPresentation() {
    final ItemPresentation presentation = myElement.getPresentation();
    assert presentation != null;
    return presentation;
  }

  @NotNull
  @Override
  public TreeElement[] getChildren() {
    fit.intellij.json.psi.JsonElement value = null;
    if (myElement instanceof fit.intellij.json.psi.JsonFile) {
      value = ((JsonFile)myElement).getTopLevelValue();
    }
    else if (myElement instanceof fit.intellij.json.psi.JsonProperty) {
      value = ((fit.intellij.json.psi.JsonProperty)myElement).getValue();
    }
    else if (PsiTreeUtil.instanceOf(myElement, fit.intellij.json.psi.JsonObject.class, fit.intellij.json.psi.JsonArray.class)) {
      value = myElement;
    }
    if (value instanceof fit.intellij.json.psi.JsonObject) {
      final fit.intellij.json.psi.JsonObject object = ((fit.intellij.json.psi.JsonObject)value);
      return ContainerUtil.map2Array(object.getPropertyList(), TreeElement.class, (Function<fit.intellij.json.psi.JsonProperty, TreeElement>) property -> new JsonStructureViewElement(property));
    }
    else if (value instanceof fit.intellij.json.psi.JsonArray) {
      final fit.intellij.json.psi.JsonArray array = (fit.intellij.json.psi.JsonArray)value;
      final List<TreeElement> childObjects = ContainerUtil.mapNotNull(array.getValueList(), value1 -> {
        if (value1 instanceof fit.intellij.json.psi.JsonObject && !((fit.intellij.json.psi.JsonObject)value1).getPropertyList().isEmpty()) {
          return new JsonStructureViewElement(value1);
        }
        else if (value1 instanceof JsonArray && PsiTreeUtil.findChildOfType(value1, fit.intellij.json.psi.JsonProperty.class) != null) {
          return new JsonStructureViewElement(value1);
        }
        return null;
      });
      return childObjects.toArray(TreeElement.EMPTY_ARRAY);
    }
    return EMPTY_ARRAY;
  }
}
