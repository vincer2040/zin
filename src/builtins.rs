use crate::object::Object;

pub fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        let err = format!("wrong number of arguments. got={}, want=1", args.len());
        return Object::Error(err);
    }
    let arg = &args[0];
    match arg {
        Object::String(s) => Object::Int(s.len() as i64),
        _ => {
            let err = format!(
                "argument to 'len' not supported, got {}",
                arg.get_type().to_string()
            );
            return Object::Error(err);
        }
    }
}
