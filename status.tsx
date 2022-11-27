import render from "preact-render-to-string";
import { FunctionComponent } from "preact";
import { setup, tw } from "twind";
import { virtualSheet, getStyleTag } from "twind/sheets";

const sheet = virtualSheet();
setup({ sheet });

export function renderStatusPage(errors: Map<string, any>): string {
  sheet.reset();
  const body = render(<Page status={errors} />);
  const styleTag = getStyleTag(sheet);

  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
${styleTag}
</head>
<body>${body}</body>
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
  <main className={tw`pt-4 px-4 max-w-4xl mx-auto flex flex-col items-center`}>
    {children}
  </main>
);

const Table: FunctionComponent<{ status: Map<string, unknown> }> = ({
  status,
}) => (
  <table
    className={tw`min-w-full table-auto border-collapse border-spacing-2 border`}
  >
    <thead className={tw`text-left`}>
      <tr className={tw`border`}>
        <th className={tw`border p-2`}>feed</th>
        <th className={tw`border p-2`}>mirror status</th>
      </tr>
    </thead>
    <tbody>
      {Array.from(status.keys()).map((key) => (
        <tr key={key} className={tw`border`}>
          <td className={tw`p-2 border hover:underline`}>
            <a
              className={tw`text-blue-600 visited:text-purple-600`}
              href={`/${key}.atom`}
              target="_blank"
            >
              /{key}.atom
            </a>
          </td>
          <td className={tw`p-2 border`}>{status.get(key) || "success"}</td>
        </tr>
      ))}
    </tbody>
  </table>
);
