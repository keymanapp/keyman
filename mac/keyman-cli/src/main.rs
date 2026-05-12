/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * `keyman` CLI entry point. Parses arguments with `clap`, dispatches
 * to the per-platform `KeymanClient`, and renders the result as text
 * or JSON. Returns a documented exit code (see README).
 */

#![forbid(unsafe_op_in_unsafe_fn)]

use std::io::{self, Write};
use std::process::ExitCode;

use clap::{Parser, Subcommand};
use keyman_cli::backend::new_client;
use keyman_cli::client::KeymanClient;
use keyman_cli::error::{CliError, Result};
use keyman_cli::keyboard::Keyboard;
use keyman_cli::output::{ActivateJson, KeyboardJson, ListJson, SelectJson, StatusJson};
use keyman_cli::resolver::{resolve_keyboard, ResolveError};

#[derive(Parser)]
#[command(
    name = "keyman",
    about = "Control Keyman from the shell.",
    version,
    propagate_version = true
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    #[command(about = "List installed Keyman keyboards available for selection")]
    List {
        #[arg(long, help = "Emit machine-readable JSON instead of text")]
        json: bool,
    },
    #[command(
        about = "Report whether Keyman is the active OS input method and which keyboard is selected"
    )]
    Status {
        #[arg(long, help = "Emit machine-readable JSON instead of text")]
        json: bool,
    },
    #[command(
        about = "Make Keyman the active OS input method without changing the selected keyboard"
    )]
    Activate {
        #[arg(long, help = "Emit machine-readable JSON instead of text")]
        json: bool,
    },
    #[command(about = "Select a Keyman keyboard (also activates Keyman as the OS input method)")]
    Select {
        #[arg(value_name = "KEYBOARD")]
        keyboard: String,
        #[arg(long, help = "Emit machine-readable JSON instead of text")]
        json: bool,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match dispatch(&cli) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            let _ = writeln!(io::stderr(), "keyman: {err}");
            ExitCode::from(u8::try_from(err.exit_code()).unwrap_or(1))
        }
    }
}

fn dispatch(cli: &Cli) -> Result<()> {
    let client = new_client();
    match &cli.command {
        Command::List { json } => cmd_list(client.as_ref(), *json),
        Command::Status { json } => cmd_status(client.as_ref(), *json),
        Command::Activate { json } => cmd_activate(client.as_ref(), *json),
        Command::Select { keyboard, json } => cmd_select(client.as_ref(), keyboard, *json),
    }
}

fn cmd_list(client: &dyn KeymanClient, json: bool) -> Result<()> {
    let keyboards = client.list_keyboards()?;
    if json {
        let payload = ListJson {
            keyboards: keyboards.iter().map(KeyboardJson::from).collect(),
        };
        write_json(&payload);
    } else {
        if keyboards.is_empty() {
            println!("(no active keyboards — install or enable one via the Keyman menu)");
            return Ok(());
        }
        let id_width = keyboards
            .iter()
            .map(|k| k.id.as_str().len())
            .max()
            .unwrap_or(0);
        for k in &keyboards {
            print_keyboard_row(k, id_width);
        }
    }
    Ok(())
}

fn cmd_status(client: &dyn KeymanClient, json: bool) -> Result<()> {
    let status = client.status()?;
    if json {
        let payload = StatusJson::from_status(&status);
        write_json(&payload);
    } else {
        println!(
            "keyman: registered as input source: {}",
            yn(status.im_state.im_registered)
        );
        println!(
            "keyman: currently selected input source: {}",
            yn(status.im_state.im_selected)
        );
        println!(
            "keyman: input-method process running: {}",
            yn(status.im_state.im_process_running)
        );
        match &status.selected_keyboard {
            Some(k) => println!("keyman: selected keyboard: {} ({})", k.name, k.id),
            None => println!("keyman: selected keyboard: (none)"),
        }
    }
    Ok(())
}

fn cmd_activate(client: &dyn KeymanClient, json: bool) -> Result<()> {
    let outcome = client.activate()?;
    if json {
        let payload = ActivateJson::from(&outcome);
        write_json(&payload);
    } else if outcome.is_noop() {
        println!("keyman: already the active input method (no change).");
    } else {
        println!(
            "keyman: registered={} selected={} (was registered={} selected={})",
            yn(outcome.im_registered_after),
            yn(outcome.im_selected_after),
            yn(outcome.im_registered_before),
            yn(outcome.im_selected_before)
        );
    }
    Ok(())
}

fn cmd_select(client: &dyn KeymanClient, input: &str, json: bool) -> Result<()> {
    let active = client.list_keyboards()?;
    let active_ids: Vec<_> = active.iter().map(|k| k.id.clone()).collect();
    let resolved = match resolve_keyboard(input, &active_ids) {
        Ok(ok) => ok.id,
        Err(ResolveError::Unknown(s)) => return Err(CliError::UnknownKeyboard { input: s }),
        Err(ResolveError::Ambiguous { input, matches }) => {
            return Err(CliError::AmbiguousKeyboard {
                input,
                matches: matches.into_iter().map(|k| k.to_string()).collect(),
            });
        }
    };

    let outcome = client.select_keyboard(&resolved)?;
    if json {
        let payload = SelectJson::from(&outcome);
        write_json(&payload);
    } else if outcome.im_activated {
        println!(
            "keyman: activated Keyman as the OS input method and selected '{}' ({})",
            outcome.selected.name, outcome.selected.id
        );
    } else {
        println!(
            "keyman: selected '{}' ({})",
            outcome.selected.name, outcome.selected.id
        );
    }
    Ok(())
}

fn print_keyboard_row(k: &Keyboard, id_width: usize) {
    let marker = if k.selected { "*" } else { " " };
    println!(
        "{marker} {:id_width$}  {}",
        k.id,
        k.name,
        id_width = id_width
    );
}

fn yn(b: bool) -> &'static str {
    if b {
        "yes"
    } else {
        "no"
    }
}

fn write_json<T: serde::Serialize>(payload: &T) {
    match serde_json::to_string_pretty(payload) {
        Ok(s) => {
            let mut stdout = io::stdout().lock();
            let _ = stdout.write_all(s.as_bytes());
            let _ = stdout.write_all(b"\n");
        }
        Err(e) => {
            let _ = writeln!(io::stderr(), "keyman: failed to serialise JSON: {e}");
        }
    }
}
