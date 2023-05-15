#include <iostream>

using namespace std;

class EmptyClass
{    
    static int a;
};
int EmptyClass::a;
class AbstractClass
{
  public: 
      virtual void funcOne() = 0;
      virtual void funcTwo() = 0;
      virtual void funcThree() = 0;
      virtual void funcecdOne() = 0;
      virtual void funcgrTwo() = 0;
      virtual void fubfrgncThree() = 0;
      virtual void fufbncOne() = 0;
      virtual void funcdfTwo() = 0;
      virtual void funcTdfhree() = 0;
      // one or any number would have size 4
};

class NotAbstrClass
{
  int uu;
  public: int virtFunc( int a){
    int b=a;
  }
};

class MixClass
{
  public:
          virtual void clFunc( int );
          static int i;
          int j;
};

int main()
{
    // Print size of class or class objects
    cout<<"Size of empty class: "<< sizeof(EmptyClass)<<endl;          
    cout<<"Size of Abstract class :"<< sizeof(AbstractClass)<<endl;
    NotAbstrClass tt;
    tt.virtFunc(5);
    cout<<"Size of Non Abstract class: "<< sizeof(NotAbstrClass)<<endl;
    cout<<"Size of Mix class: "<< sizeof(MixClass)<<endl;
    return 0;
}