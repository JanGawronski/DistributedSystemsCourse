fn main() {
    // proto file lives in the repository's top-level proto/ directory
    tonic_build::compile_protos("../proto/devices.proto").unwrap();
}
