package fit.intellij.json.psi.impl;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.util.PsiTreeUtil;
import fit.intellij.json.psi.JsonFile;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class JsonFileImpl extends PsiFileBase implements JsonFile {

  public JsonFileImpl(FileViewProvider fileViewProvider, Language language) {
    super(fileViewProvider, language);
  }

  @NotNull
  @Override
  public FileType getFileType() {
    return getViewProvider().getFileType();
  }

  @Nullable
  @Override
  public fit.intellij.json.psi.JsonValue getTopLevelValue() {
    return PsiTreeUtil.getChildOfType(this, fit.intellij.json.psi.JsonValue.class);
  }

  @NotNull
  @Override
  public List<fit.intellij.json.psi.JsonValue> getAllTopLevelValues() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, JsonValue.class);
  }

  @Override
  public String toString() {
    return "JsonFile: " + getName();
  }
}
