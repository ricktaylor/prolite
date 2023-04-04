use std::{env, fs::OpenOptions, io, path::{PathBuf, Path}};

use super::*;
use text::*;

struct FSResolver {
    root: PathBuf,
}

impl FSResolver {
    fn new(root: &str) -> io::Result<FSResolver> {
        Ok(Self {
            root: env::current_dir()?.join(root).canonicalize()?,
        })
    }
}

impl StreamResolver for FSResolver {
    fn open(
        &mut self,
        name: &str,
    ) -> Result<(String, Box<dyn stream::ReadStream>), StreamResolverError> {
        let mut fp = self.root.join(name);
        let mut f = OpenOptions::new().read(true).open(&fp);
        if let Err(ref e) = f {
            match e.kind() {
                std::io::ErrorKind::NotFound if matches!(fp.extension(), None) => {
                    fp.set_extension("pl");
                    f = OpenOptions::new().read(true).open(&fp);
                }
                _ => {}
            }
        }

        match f {
            Err(e) => Err(StreamResolverError {
                error: e,
                path: fp.into_os_string().into_string().unwrap(),
            }),
            Ok(f) => {
                let n = fp
                    .canonicalize()
                    .unwrap()
                    .into_os_string()
                    .into_string()
                    .unwrap();
                println!("Opened {}", n);

                let r = Box::new(read_term::utf8reader::Utf8Reader::new(f, &n));
                Ok((n, r))
            }
        }
    }
}

fn error(e: &error::Error) -> bool {
    println!("{:?}", e);
    true
}

fn test_file(source: &str) {
    let p = Path::new(source);
    let mut res = FSResolver::new(p.parent().unwrap().to_str().unwrap()).unwrap();
    consult(&mut res, p.file_name().unwrap().to_str().unwrap(), Some(error)).unwrap();
}

#[test]
fn test_consult() {
    test_file("./test/vanilla/vanilla.pl");
    test_file("./test/inriasuite/inriasuite.pl");
}

