use std::error::Error;
use std::io::{self, Write};
use tonic::transport::Channel;
use tonic::Request;

pub mod devices {
    tonic::include_proto!("smart.home");
}

use devices::device_manager_client::DeviceManagerClient;
use devices::{
    CameraCommand, CommandRequest, DeviceId, FridgeCommand, HeaterCommand, RecordAction, SetPtz,
    SetSetpoint, SetTemperature, ToggleDoor, TurnOnOff,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    println!("Smart Home gRPC client (Rust + tonic)");

    let mut endpoint = "http://127.0.0.1:50051".to_string();

    loop {
        println!("\nCurrent server endpoint: {}", endpoint);
        println!(
            "1) List devices
2) Get device info
3) Get device state
4) Fridge: set temperature
5) Fridge: set door open/closed
6) Heater: set setpoint
7) Heater: turn on/off
8) Camera: set PTZ
9) Camera: start/stop recording
10) Stream device updates
11) Change server endpoint
0) Exit"
        );

        let choice = read_line("> ")?;

        let result = match choice.as_str() {
            "1" => list_devices(&endpoint).await,
            "2" => get_device_info(&endpoint).await,
            "3" => get_device_state(&endpoint).await,
            "4" => fridge_set_temp(&endpoint).await,
            "5" => fridge_set_door(&endpoint).await,
            "6" => heater_set_setpoint(&endpoint).await,
            "7" => heater_set_onoff(&endpoint).await,
            "8" => camera_set_ptz(&endpoint).await,
            "9" => camera_set_recording(&endpoint).await,
            "10" => stream_device_updates(&endpoint).await,
            "11" => {
                let new = read_line("New endpoint (e.g. http://127.0.0.1:50051): ")?;
                endpoint = new;
                Ok(())
            }
            "0" => break,
            _ => {
                println!("Unknown option");
                Ok(())
            }
        };

        if let Err(err) = result {
            eprintln!("Error: {err}");
        }
    }

    Ok(())
}

async fn connect(endpoint: &str) -> Result<DeviceManagerClient<Channel>, Box<dyn Error>> {
    Ok(DeviceManagerClient::connect(endpoint.to_string()).await?)
}

async fn list_devices(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let resp = client.list_devices(Request::new(())).await?;
    println!("Devices:");
    for device in resp.into_inner().devices {
        println!(
            "- {} [{} / {}] @ {}",
            device.id, device.device_type, device.subtype, device.location
        );
    }
    Ok(())
}

async fn get_device_info(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Device id: ")?;
    let resp = client
        .get_device_info(Request::new(DeviceId { id }))
        .await?;
    println!("Device info: {:?}", resp.into_inner());
    Ok(())
}

async fn get_device_state(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Device id: ")?;
    let resp = client
        .get_device_state(Request::new(DeviceId { id }))
        .await?;
    println!("State: {:?}", resp.into_inner());
    Ok(())
}

async fn fridge_set_temp(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Fridge id: ")?;
    let temperature = read_f64("Temperature (C): ", 4.0)?;
    let fridge_cmd = FridgeCommand {
        action: Some(devices::fridge_command::Action::SetTemp(SetTemperature {
            temperature,
        })),
    };
    let req = Request::new(CommandRequest {
        id,
        cmd: Some(devices::command_request::Cmd::Fridge(fridge_cmd)),
    });
    let resp = client.send_command(req).await?;
    println!("Response: {:?}", resp.into_inner());
    Ok(())
}

async fn fridge_set_door(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Fridge id: ")?;
    let open = read_bool("Door open? [y/n]: ")?;
    let fridge_cmd = FridgeCommand {
        action: Some(devices::fridge_command::Action::Door(ToggleDoor { open })),
    };
    let req = Request::new(CommandRequest {
        id,
        cmd: Some(devices::command_request::Cmd::Fridge(fridge_cmd)),
    });
    let resp = client.send_command(req).await?;
    println!("Response: {:?}", resp.into_inner());
    Ok(())
}

async fn heater_set_setpoint(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Heater id: ")?;
    let setpoint = read_f64("Setpoint (C): ", 22.0)?;
    let heater_cmd = HeaterCommand {
        action: Some(devices::heater_command::Action::Setpoint(SetSetpoint {
            setpoint,
        })),
    };
    let req = Request::new(CommandRequest {
        id,
        cmd: Some(devices::command_request::Cmd::Heater(heater_cmd)),
    });
    let resp = client.send_command(req).await?;
    println!("Response: {:?}", resp.into_inner());
    Ok(())
}

async fn heater_set_onoff(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Heater id: ")?;
    let on = read_bool("Turn heater on? [y/n]: ")?;
    let heater_cmd = HeaterCommand {
        action: Some(devices::heater_command::Action::Onoff(TurnOnOff { on })),
    };
    let req = Request::new(CommandRequest {
        id,
        cmd: Some(devices::command_request::Cmd::Heater(heater_cmd)),
    });
    let resp = client.send_command(req).await?;
    println!("Response: {:?}", resp.into_inner());
    Ok(())
}

async fn camera_set_ptz(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Camera id: ")?;
    let pan = read_f64("Pan (deg): ", 0.0)?;
    let tilt = read_f64("Tilt (deg): ", 0.0)?;
    let zoom = read_f64("Zoom: ", 1.0)?;
    let camera_cmd = CameraCommand {
        action: Some(devices::camera_command::Action::Ptz(SetPtz {
            pan,
            tilt,
            zoom,
        })),
    };
    let req = Request::new(CommandRequest {
        id,
        cmd: Some(devices::command_request::Cmd::Camera(camera_cmd)),
    });
    let resp = client.send_command(req).await?;
    println!("Response: {:?}", resp.into_inner());
    Ok(())
}

async fn camera_set_recording(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Camera id: ")?;
    let start = read_bool("Start recording? [y/n]: ")?;
    let camera_cmd = CameraCommand {
        action: Some(devices::camera_command::Action::Record(RecordAction {
            start,
        })),
    };
    let req = Request::new(CommandRequest {
        id,
        cmd: Some(devices::command_request::Cmd::Camera(camera_cmd)),
    });
    let resp = client.send_command(req).await?;
    println!("Response: {:?}", resp.into_inner());
    Ok(())
}

async fn stream_device_updates(endpoint: &str) -> Result<(), Box<dyn Error>> {
    let mut client = connect(endpoint).await?;
    let id = read_line("Device id to stream: ")?;
    let mut stream = client
        .stream_device_state(Request::new(DeviceId { id }))
        .await?
        .into_inner();
    println!("Streaming updates (Ctrl-C to stop client) ...");
    while let Some(update) = stream.message().await? {
        println!("Update: {:?}", update);
    }
    Ok(())
}

fn read_line(prompt: &str) -> Result<String, Box<dyn Error>> {
    print!("{prompt}");
    io::stdout().flush()?;
    let mut s = String::new();
    io::stdin().read_line(&mut s)?;
    Ok(s.trim().to_string())
}

fn read_f64(prompt: &str, default: f64) -> Result<f64, Box<dyn Error>> {
    let raw = read_line(prompt)?;
    Ok(raw.parse::<f64>().unwrap_or(default))
}

fn read_bool(prompt: &str) -> Result<bool, Box<dyn Error>> {
    loop {
        let raw = read_line(prompt)?;
        match raw.to_lowercase().as_str() {
            "y" | "yes" | "1" | "true" | "on" => return Ok(true),
            "n" | "no" | "0" | "false" | "off" => return Ok(false),
            _ => println!("Please enter y/n."),
        }
    }
}
