// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// automatically generated weight registration

#include <cmath>
#include <string>
#include <vector>

#include <iostream>
#include <iomanip>
#include <stdexcept>

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"

#include "hashtable/entry.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/attributes.h"
#include "thesaurus/weight.h"
#include "thesaurus/relation.h"
#include "thesaurus/object.h"
#include "thesaurus/objects.h"
#include "thesaurus/weights.h"


namespace NLP {
  namespace Thesaurus {

    class WIdentity: public Weight {
    public:
      WIdentity(void): Weight("identity", "Identity", "1.0") {};
      ~WIdentity(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WZero: public Weight {
    public:
      WZero(void): Weight("zero", "Zero", "0.0") {};
      ~WZero(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WFreq: public Weight {
    public:
      WFreq(void): Weight("freq", "Freq", "f(r)") {};
      ~WFreq(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WRelFreq: public Weight {
    public:
      WRelFreq(void): Weight("relfreq", "RelFreq", "f(r)/f(o)") {};
      ~WRelFreq(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}

#include "thesaurus/weights/chi2.h"

#include "thesaurus/weights/lr.h"

#include "thesaurus/weights/lin98b.h"

#include "thesaurus/weights/lin98c.h"


namespace NLP {
  namespace Thesaurus {

    class WDice: public Weight {
    public:
      WDice(void): Weight("dice", "Dice", "2*p(r)/(p(o) + p(a))") {};
      ~WDice(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WDiceLog: public Weight {
    public:
      WDiceLog(void): Weight("dicelog", "DiceLog", "2*log2(f(r) + 1)*p(r)/(p(o) + p(a))") {};
      ~WDiceLog(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WGrefenstette: public Weight {
    public:
      WGrefenstette(void): Weight("grefenstette", "Grefenstette", "log2(f(r) + 1)/log2(n(a) + 1)") {};
      ~WGrefenstette(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WMI: public Weight {
    public:
      WMI(void): Weight("mi", "MI", "log2(p(r)/(p(o)*p(a)))") {};
      ~WMI(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WMILog: public Weight {
    public:
      WMILog(void): Weight("milog", "MILog", "log2(f(r) + 1)*log2(p(r)/(p(o)*p(a)))") {};
      ~WMILog(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WMICut: public Weight {
    public:
      WMICut(void): Weight("micut", "MICut", "cut(log2(p(r)/(p(o)*p(a))), 0)") {};
      ~WMICut(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WMICutLog: public Weight {
    public:
      WMICutLog(void): Weight("micutlog", "MICutLog", "cut(log2(p(r)/(p(o)*p(a))), 0)*log2(f(r) + 1)") {};
      ~WMICutLog(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WTTest: public Weight {
    public:
      WTTest(void): Weight("ttest", "TTest", "(p(r) - p(a)*p(o))/sqrt(p(a)*p(o))") {};
      ~WTTest(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WTTestLog: public Weight {
    public:
      WTTestLog(void): Weight("ttestlog", "TTestLog", "log2(f(r) + 1)*(p(r) - p(a)*p(o))/sqrt(p(a)*p(o))") {};
      ~WTTestLog(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WTTestCut: public Weight {
    public:
      WTTestCut(void): Weight("ttestcut", "TTestCut", "cut(p(r) - p(a)*p(o), 0)/sqrt(p(a)*p(o))") {};
      ~WTTestCut(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WTTestCutLog: public Weight {
    public:
      WTTestCutLog(void): Weight("ttestcutlog", "TTestCutLog", "cut(p(r) - p(a)*p(o), 0)*log2(f(r) + 1)/sqrt(p(a)*p(o))") {};
      ~WTTestCutLog(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}

#include "thesaurus/weights/relweight.h"


namespace NLP {
  namespace Thesaurus {

    class WTFIDF: public Weight {
    public:
      WTFIDF(void): Weight("tfidf", "TFIDF", "f(r)/n(a)") {};
      ~WTFIDF(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP {
  namespace Thesaurus {

    class WTFIDF2: public Weight {
    public:
      WTFIDF2(void): Weight("tfidf2", "TFIDF2", "log2(f(r) + 1)/log2(1 + 1/q(a))") {};
      ~WTFIDF2(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}


namespace NLP { namespace Thesaurus {

WeightRegistry::WeightRegistry(void){
      install(new WIdentity);
      install(new WZero);
      install(new WFreq);
      install(new WRelFreq);
      install(new WChi2);
      install(new WLR);
      install(new WLin98b);
      install(new WLin98c);
      install(new WDice);
      install(new WDiceLog);
      install(new WGrefenstette);
      install(new WMI);
      install(new WMILog);
      install(new WMICut);
      install(new WMICutLog);
      install(new WTTest);
      install(new WTTestLog);
      install(new WTTestCut);
      install(new WTTestCutLog);
      install(new WRelWeight);
      install(new WTFIDF);
      install(new WTFIDF2);
}

WeightRegistry weights;

} }

