use typescript_services_rust::{initialize_node_system, os_platform, start};

fn main() {
    // setStackTraceLimit();
    // if (typeof process !== "undefined") {
    start(initialize_node_system(), os_platform());
    // }
    // else {
    //     const listener = (e: any) => {
    //         removeEventListener("message", listener);
    //         const args = e.data;
    //         start(initializeWebSystem(args), "web");
    //     };
    //     addEventListener("message", listener);
    // }
}
