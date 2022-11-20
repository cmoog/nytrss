export function renderStatusPage(errors: Map<string, any>): string {
  return `
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link rel="stylesheet" href="style.css">
</head>
	<body>
		<table>
		<thead>
			<td>route</td>
			<td>status</td>
		</thead>
		${Array.from(errors.keys())
      .map((key) => {
        const err = errors.get(key);
        return `<tr>
				<td>${key}</td>
				<td>${err === null ? "success" : "error"}</td>
			</tr>`;
      })
      .join("\n")}
		</table>
	</body>
</html>
`;
}
