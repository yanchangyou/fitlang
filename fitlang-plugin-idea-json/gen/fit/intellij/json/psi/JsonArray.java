// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.navigation.ItemPresentation;

public interface JsonArray extends JsonContainer {

  @NotNull
  List<JsonValue> getValueList();

  @Nullable ItemPresentation getPresentation();

}
