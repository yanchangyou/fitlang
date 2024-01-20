package fit.intellij.json.structureView;

import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.StructureViewModelBase;
import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.Sorter;
import fit.intellij.json.psi.JsonFile;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiFile;
import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonObject;
import fit.intellij.json.psi.JsonProperty;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author Mikhail Golubev
 */
public class JsonStructureViewModel extends StructureViewModelBase implements StructureViewModel.ElementInfoProvider {

  public JsonStructureViewModel(@NotNull PsiFile psiFile, @Nullable Editor editor) {
    super(psiFile, editor, new JsonStructureViewElement((JsonFile)psiFile));
    withSuitableClasses(JsonFile.class, JsonProperty.class, JsonObject.class, JsonArray.class);
    withSorters(Sorter.ALPHA_SORTER);
  }

  @Override
  public boolean isAlwaysShowsPlus(StructureViewTreeElement element) {
    return false;
  }

  @Override
  public boolean isAlwaysLeaf(StructureViewTreeElement element) {
    return false;
  }

}
