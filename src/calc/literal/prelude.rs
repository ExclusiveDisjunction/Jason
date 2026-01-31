use crate::calc::err::UndefinedBiOperation;

pub trait LogicalCmp {
    fn oper_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation>;
    fn oper_neq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        self.oper_eq(rhs).map(|x| !x ).map_err(|x| x.rename("!="))
    }
    fn oper_less(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation>;
    fn oper_less_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match self.oper_eq(rhs) {
            Ok(v) => Ok( v || self.oper_less(rhs)? ),
            Err(e) => Err( e.rename("<=") )
        }
    }
    fn oper_greater(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation>;
    fn oper_greater_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match self.oper_eq(rhs) {
            Ok(v) => Ok( v || self.oper_greater(rhs)? ),
            Err(e) => Err( e.rename(">=") )
        }
    }
}