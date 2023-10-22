package fit.intellij.json.findUsages;

import fit.intellij.json.JsonElementTypes;
import com.intellij.lang.cacheBuilder.DefaultWordsScanner;
import com.intellij.psi.tree.TokenSet;
import fit.intellij.json.JsonLexer;
import fit.intellij.json.JsonParserDefinition;

/**
 * @author Mikhail Golubev
 */
public class JsonWordScanner extends DefaultWordsScanner {
  public JsonWordScanner() {
    super(new JsonLexer(), TokenSet.create(JsonElementTypes.IDENTIFIER), JsonParserDefinition.JSON_COMMENTARIES, JsonParserDefinition.JSON_LITERALS);
    setMayHaveFileRefsInLiterals(true);
  }
}
