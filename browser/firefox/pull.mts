// Pulls live Tree Style Tab and Vimium settings from a Firefox profile into this directory.
// Both extensions mirror their settings into storage.sync, which Firefox keeps as plain JSON.
import { DatabaseSync } from "node:sqlite";
import { copyFileSync, existsSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { join } from "node:path";

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };
type Settings = Record<string, Json>;

const profile: string =
  process.argv[2] ||
  join(
    homedir(),
    process.platform === "darwin"
      ? "Library/Application Support/Firefox/Profiles/default"
      : ".mozilla/firefox/default",
  );

// Firefox holds the DB open; copying it with its WAL picks up writes not yet checkpointed
const tmp = mkdtempSync(join(tmpdir(), "firefox-pull-"));
for (const f of ["storage-sync-v2.sqlite", "storage-sync-v2.sqlite-wal"]) {
  if (existsSync(join(profile, f))) copyFileSync(join(profile, f), join(tmp, f));
}
const db = new DatabaseSync(join(tmp, "storage-sync-v2.sqlite"));
const sync = (id: string): Settings => {
  const row = db.prepare("select data from storage_sync_data where ext_id = ?").get(id) as { data: string } | undefined;
  return JSON.parse(row?.data ?? "{}");
};
const tst = sync("treestyletab@piro.sakura.ne.jp");
const vimium = sync("{d7742d87-e61d-4b78-b8a1-b469842139fa}");
db.close();
rmSync(tmp, { recursive: true });

const sortKeys = (_: string, v: Json): Json =>
  v && typeof v === "object" && !Array.isArray(v)
    ? Object.fromEntries(Object.entries(v).sort(([a], [b]) => (a < b ? -1 : 1)))
    : v;
const write = (file: string, text: string) => writeFileSync(join(import.meta.dirname, file), text);
const writeJson = (file: string, value: Json) => write(file, JSON.stringify(value, sortKeys, 2) + "\n");

const css = Array.from({ length: 8 }, (_, i) => tst[`chunkedUserStyleRules${i}`] ?? "").join("");
if (css) write("chrome/tree_style_tab.css", Buffer.from(css, "base64").toString());

// TST runtime state stored next to its settings
const state =
  /^(configsVersion|__ConfigsMigration__.*|syncDevices|.*NotificationShown|userStyleRules\d*|chunkedUserStyleRules\d+)$/;
writeJson("tst-settings.json", Object.fromEntries(Object.entries(tst).filter(([k]) => !state.test(k))));
writeJson("vimium-options.json", vimium);
