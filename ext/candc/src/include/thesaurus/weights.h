/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.


namespace NLP {
  namespace Thesaurus {

    class WeightRegistry {
    private:
      std::vector<Weight *> _entries;
    public:
      WeightRegistry(void);

      ~WeightRegistry(void){
        for(vector<Weight *>::iterator i = _entries.begin(); i != _entries.end(); i++)
          delete *i;
      }

      void install(Weight *entry){ _entries.push_back(entry); };
      Weight &get(const std::string &name){
        for(vector<Weight *>::iterator i = _entries.begin(); i != _entries.end(); i++)
          if(name == (*i)->NAME)
            return **i;
        throw NLP::Exception("weight function '" + name + "' does not exist");
      };

      std::ostream &print(std::ostream &out) const {
        for(vector<Weight *>::const_iterator i = _entries.begin(); i != _entries.end(); ++i)
          out << (*i)->NAME << endl;
        return out;
      };
    };

    inline std::ostream &operator<<(std::ostream &out, const WeightRegistry &measures){
      return measures.print(out);
    }

  }
}
