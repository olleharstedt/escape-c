module To_test = struct
    let lowercase = String.lowercase_ascii
end

let test_lowercase = Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let () =
    let open Alcotest in
    run "Utils" [ 
        "string-case", [
            test_case "Lower case"     `Quick test_lowercase;
        ]
    ]
