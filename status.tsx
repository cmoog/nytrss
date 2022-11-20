import render from "preact-render-to-string";
import { FunctionComponent } from "preact";

export function renderStatusPage(errors: Map<string, any>): string {
  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<script src="https://cdn.tailwindcss.com"></script>
</head>
<body>${render(<Page status={errors} />)}</body>
</html>
`;
}

const Page: FunctionComponent<{ status: Map<string, unknown> }> = ({
  status,
}) => (
  <Layout>
    <Table status={status} />
  </Layout>
);

const Layout: FunctionComponent = ({ children }) => (
  <main className="pt-4 px-4 max-w-4xl mx-auto flex flex-col items-center">
    {children}
  </main>
);

const Table: FunctionComponent<{ status: Map<string, unknown> }> = ({
  status,
}) => (
  <table className="min-w-full table-auto border-collapse border-spacing-2 border">
    <thead className="text-left">
      <tr className="border">
        <th className="border p-2">feed</th>
        <th className="border p-2">mirror status</th>
      </tr>
    </thead>
    <tbody>
      {Array.from(status.keys()).map((key) => (
        <tr key={key} className="border">
          <td className="p-2 border hover:underline">
            <a
              className="text-blue-600 visited:text-purple-600"
              href={`/${key}`}
              target="_blank"
            >
              /{key}
            </a>
          </td>
          <td className="p-2 border">{status.get(key) || "success"}</td>
        </tr>
      ))}
    </tbody>
  </table>
);
