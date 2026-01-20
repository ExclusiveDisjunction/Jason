use crate::calc::err::UndefinedBiOperation;

pub trait LogicalCmp {
    fn oper_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation>;
    fn oper_neq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        self.oper_eq(rhs).map(|x| !x ).map_err(|x| x.rename("!="))
    }
    fn oper_less(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation>;
    fn oper_less_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match self.oper_less(rhs) {
            Ok(v) => Ok( v || self.oper_eq(rhs).unwrap() ), //Since oper_eq will only return an error if the types are not matching, and oper_less will only return if both are Sca.. this will never return an error.
            Err(e) => Err( e.rename("<=") )
        }
    }
    fn oper_greater(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation>;
    fn oper_greater_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match self.oper_greater(rhs) {
            Ok(v) => Ok( v || self.oper_eq(rhs).unwrap()),
            Err(e) => Err( e.rename(">=") )
        }
    }
}