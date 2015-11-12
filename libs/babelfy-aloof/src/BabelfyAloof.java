import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import it.uniroma1.lcl.babelfy.commons.*;
import it.uniroma1.lcl.babelfy.commons.BabelfyParameters.MCS;
import it.uniroma1.lcl.babelfy.commons.BabelfyParameters.ScoredCandidates;
import it.uniroma1.lcl.babelfy.commons.BabelfyParameters.SemanticAnnotationResource;
import it.uniroma1.lcl.babelfy.commons.annotation.*;
import it.uniroma1.lcl.babelfy.commons.annotation.SemanticAnnotation.Source;
import it.uniroma1.lcl.babelfy.core.Babelfy;
import it.uniroma1.lcl.jlt.util.Language;

import it.uniroma1.lcl.babelnet.BabelNet;
import it.uniroma1.lcl.babelnet.BabelSynset;
import it.uniroma1.lcl.babelnet.BabelSynsetID;
import it.uniroma1.lcl.babelnet.resources.WordNetSynsetID;

public class BabelfyAloof {
	public static void main(String[] args) {
		List<String> str_tokens = new ArrayList<String>();
		List<BabelfyToken> tokens = new ArrayList<BabelfyToken>();
		try {
			String[] parts = args[0].replace('\n', ' ').split(" ");
			str_tokens = Arrays.asList(parts);
		}
		catch (Exception e) {

		}
		for (String str_token : str_tokens) {
			BabelfyToken token = new BabelfyToken(str_token);
			tokens.add(token);
		}

		BabelfyParameters bp = new BabelfyParameters();
		bp.setAnnotationResource(SemanticAnnotationResource.BN);
		bp.setMCS(MCS.ON_WITH_STOPWORDS);
		bp.setScoredCandidates(ScoredCandidates.TOP);
		Babelfy bfy = new Babelfy(bp);
		List<SemanticAnnotation> bfyAnnotations = bfy.babelfy(tokens, Language.EN);

		for (SemanticAnnotation annotation : bfyAnnotations)
		{
			String bsid_str = annotation.getBabelSynsetID();

			BabelSynsetID bsid = new BabelSynsetID(bsid_str);
			BabelNet bn = BabelNet.getInstance();
			BabelSynset bs = bn.getSynset(bsid);
			List<WordNetSynsetID> offsets = bs.getWordNetOffsets();

			System.out.println(annotation.getTokenOffsetFragment().getStart() +
								"\t" + annotation.getTokenOffsetFragment().getEnd() +
								"\t" + annotation.getBabelNetURL() +
								"\t" + offsets.get(0) +
								"\t" + annotation.getDBpediaURL());
		}
	}
}
