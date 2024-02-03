package fit.intellij.json.findUsages;

import fit.intellij.json.JsonLexer;
import com.intellij.lang.cacheBuilder.DefaultWordsScanner;
import com.intellij.psi.tree.TokenSet;
import fit.intellij.json.JsonElementTypes;

import static fit.intellij.json.JsonTokenSets.JSON_COMMENTARIES;
import static fit.intellij.json.JsonTokenSets.JSON_LITERALS;

/**
 * @author Mikhail Golubev
 */
public class JsonWordScanner extends DefaultWordsScanner {
  public JsonWordScanner() {
    super(new JsonLexer(), TokenSet.create(JsonElementTypes.IDENTIFIER), JSON_COMMENTARIES, JSON_LITERALS);
    setMayHaveFileRefsInLiterals(true);
  }
}
