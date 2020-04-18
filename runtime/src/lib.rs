use libc;
use std::io::Write;
/// - ret.insert_builtin_func("puti");
/// - ret.insert_builtin_func("putb");
/// - ret.insert_builtin_func("putc");
/// - ret.insert_builtin_func("puts");
/// - ret.insert_builtin_func("geti");
/// - ret.insert_builtin_func("getb");
/// - ret.insert_builtin_func("getc");
/// - ret.insert_builtin_func("gets");
/// - ret.insert_builtin_func("abs");
/// - ret.insert_builtin_func("ord");
/// - ret.insert_builtin_func("chr");
/// - ret.insert_builtin_func("strlen");
/// - ret.insert_builtin_func("strcmp");
/// - ret.insert_builtin_func("strcpy");
/// - ret.insert_builtin_func("strcat");
/// - ret.insert_builtin_func("readInteger");
/// - ret.insert_builtin_func("nil");
/// - ret.insert_builtin_func("nil?");
/// - ret.insert_builtin_func("head");
/// - ret.insert_builtin_func("tail");
/// - ret.insert_builtin_func("cons");
///
///
///
///

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

/// # Βιβλιοθήκη έτοιμων συναρτήσεων
/// Η Tony υποστηρίζει ένα σύνολο προκαθορισμένων δομικών μονάδων, οι οποίες έχουν υλοποιηθεί σε
/// assembly του x86 ως μια βιβλιοθήκη έτοιμων συναρτήσεων (run-time library). Είναι ορατές σε κάθε
/// δο- μική μονάδα, εκτός αν επισκιάζονται από μεταβλητές, παραμέτρους ή άλλες δομικές μονάδες με
/// το ίδιο όνομα. Παρακάτω δίνονται οι δηλώσεις τους και εξηγείται η λειτουργία τους.
/// ## Είσοδος και έξοδος
/// ```ignore
/// decl puti(int n)
/// decl putb(bool b)
/// decl putc(char c)
/// decl puts(char[] s)
/// ```
#[no_mangle]
pub extern "C" fn puti(x: i64) -> () {
    print_flush!("{}", x);
}

#[no_mangle]
pub extern "C" fn putb(b: bool) -> () {
    print_flush!("{}", if b { "true" } else { "false" });
}

#[no_mangle]
pub extern "C" fn putc(c: std::os::raw::c_int) -> () {
    unsafe {
        libc::putchar(c);
    };
}

#[no_mangle]
pub extern "C" fn puts(c: *const std::os::raw::c_char) -> () {
    unsafe {
        libc::printf(
            std::ffi::CString::new("%s")
                .expect("CString::new failed")
                .as_ptr(),
            c,
        );
    };
}

/// Οι συναρτήσεις αυτές χρησιμοποιούνται για την εκτύπωση τιμών που ανήκουν στους βασικούς τύπους
/// της Tony , καθώς και για την εκτύπωση συμβολοσειρών.
///τήσεις διαχείρισης συμβολοσειρών
/// ```ignore
/// decl int geti ()
/// decl bool getb ()
/// decl char getc ()
/// decl gets (int n, char[] s)
/// ```
///
/// Αντίστοιχα, οι παραπάνω συναρτήσεις χρησιμοποιούνται για την εισαγωγή τιμών που ανήκουν στους
/// βασικούς τύπους της Tony και για την εισαγωγή συμβολοσειρών. Η συνάρτηση gets χρησιμοποιείται
/// για την ανάγνωση μιας συμβολοσειράς μέχρι τον επόμενο χαρακτήρα αλλαγής γραμμής. Οι παράμετροι
/// της καθορίζουν το μέγιστο αριθμό χαρακτήρων (συμπεριλαμβανομένου του τελικού ’ \ 0’ ) που
/// επιτρέπεται να διαβαστούν και τον πίνακα χαρακτήρων στον οποίο αυτοί θα τοποθετηθούν. Ο
/// χαρακτήρας αλλαγής γραμμής δεν αποθηκεύεται. Αν το μέγεθος του πίνακα εξαντληθεί πριν
/// συναντηθεί χαρακτήρας αλλαγής γραμμής, η ανάγνωση θα συνεχιστεί αργότερα από το σημείο όπου
/// διακόπηκε.
///
/// ##Συναρτήτήσεις διαχείρισης συμβολοσειρών
/// ```ignore
/// decl int abs (int n)
/// decl int ord (char c)
/// decl char chr (int n)
/// ```
///
/// H συνάρτηση abs υπολογίζει την απόλυτη τιμή ενός ακέραιου αριθμού. Οι συναρτήσεις ord και chr
/// μετατρέπουν από ένα χαρακτήρα στον αντίστοιχο κωδικό ASCII και αντίστροφα.
///
#[no_mangle]
pub extern "C" fn abs(x: i64) -> i64 {
    x.abs()
}
#[no_mangle]
pub extern "C" fn ord(c: char) -> i64 {
    c as u32 as i64
}
#[no_mangle]
pub extern "C" fn chr(i: i64) -> char {
    i as i64 as u8 as char
}

/// ## Συναρτήσεις διαχείρισης συμβολοσειρών
/// ```ignore
/// decl int strlen (char[] s)
/// decl int strcmp (char[] s1, s2)
/// decl strcpy (char[] trg, src)
/// decl strcat (char[] trg, src)
/// ```
/// Οι συναρτήσεις αυτές έχουν ακριβώς την ίδια λειτουργία με τις συνώνυμές τους στη βιβλιοθήκη
/// συναρτήσεων της γλώσσας C.

#[no_mangle]
pub extern "C" fn strlen(c: *const std::os::raw::c_char) -> i64 {
    (unsafe { libc::strlen(c) }) as i64
}

#[no_mangle]
pub extern "C" fn strcmp(s1: *const std::os::raw::c_char, s2: *const std::os::raw::c_char) -> i64 {
    (unsafe { libc::strcmp(s1, s2) }) as i64
}

#[no_mangle]
pub extern "C" fn strcpy(trg: *mut std::os::raw::c_char, src: *const std::os::raw::c_char) -> () {
    unsafe { libc::strcpy(trg, src) };
}

#[no_mangle]
pub extern "C" fn strcat(trg: *mut std::os::raw::c_char, src: *const std::os::raw::c_char) -> () {
    unsafe { libc::strcat(trg, src) };
}

//#[no_mangle]
//pub extern "C" fn putchard(x: i64) -> i64 {
//    print_flush!("{}", x as u8 as char);
//    x
//}
//
//#[no_mangle]
//pub extern "C" fn printd(x: i64) -> i64 {
//    println!("{}", x);
//    x
//}
//
//// Adding the functions above to a global array,
//// so Rust compiler won't remove them.
//#[used]
//static EXTERNAL_FNS: [extern "C" fn(i64) -> ;64; 2] = [putchard, printd];
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
