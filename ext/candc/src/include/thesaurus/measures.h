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

    class MeasureRegistry {
    private:
      std::vector<Measure *> _entries;
    public:
      MeasureRegistry(void);
      ~MeasureRegistry(void){
        for(vector<Measure *>::iterator i = _entries.begin(); i != _entries.end(); i++)
          delete *i;
      }

      void install(Measure *entry){
        _entries.push_back(entry);
      };
      Measure *get(const std::string &name){
        for(vector<Measure *>::iterator i = _entries.begin(); i != _entries.end(); i++)
          if(name == (*i)->NAME)
            return *i;
        throw NLP::Exception("measure function '" + name + "' does not exist");
      };

      std::ostream &print(std::ostream &out) const {
        for(vector<Measure *>::const_iterator i = _entries.begin(); i != _entries.end(); ++i)
          out << (*i)->NAME << endl;
        return out;
      };
    };

    inline std::ostream &operator<<(std::ostream &out, const MeasureRegistry &measures){
      return measures.print(out);
    }

  }
}
