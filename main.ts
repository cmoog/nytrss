import { Feed } from "feed";
import { parseFeed } from "htmlparser2";
import { load } from "cheerio";
import { renderStatusPage } from "./status";

export default {
  fetch: handler,
  scheduled,
};

type Extractor = (articleUrl: string) => Promise<string | null>;

async function fetchHtml(url: string): Promise<string> {
  const resp = await fetch(url, fakeAgent);
  if (!resp.ok) {
    throw Error("http error: " + resp.status);
  }
  const html = await resp.text();
  if (html.includes("Completing the CAPTCHA proves you are a human")) {
    throw Error("rate limited");
  }
  return html;
}

const NytExtractor: Extractor = async (articleUrl) => {
  const html = await fetchHtml(articleUrl);
  return selectElement(html, `section[name='articleBody']`);
};

const WsjExtractor: Extractor = async (articleUrl) => {
  // return selectElement(html, `div.crawler > section`);
  const url = `https://archive.ph/oldest/${articleUrl}`;
  const html = await fetchHtml(url);
  return selectElement(html, `section[subscriptions-section='content']`);
};

type Feeds = Record<
  string,
  {
    source: string;
    extractor: Extractor;
  }
>;

const feeds: Feeds = {
  "nyt/politics": {
    source: "https://rss.nytimes.com/services/xml/rss/nyt/Politics.xml",
    extractor: NytExtractor,
  },
  "nyt/business": {
    source: "https://rss.nytimes.com/services/xml/rss/nyt/Business.xml",
    extractor: NytExtractor,
  },
  "nyt/economy": {
    source: "https://rss.nytimes.com/services/xml/rss/nyt/Economy.xml",
    extractor: NytExtractor,
  },
  "nyt/technology": {
    source: "https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml",
    extractor: NytExtractor,
  },
  "nyt/us": {
    source: "https://rss.nytimes.com/services/xml/rss/nyt/US.xml",
    extractor: NytExtractor,
  },
  "nyt/opinion/ezra-klein": {
    source:
      "https://www.nytimes.com/svc/collections/v1/publish/www.nytimes.com/column/ezra-klein/rss.xml",
    extractor: NytExtractor,
  },
  // "wsj/markets": {
  //   source: "https://feeds.a.dj.com/rss/RSSMarketsMain.xml",
  //   extractor: WsjExtractor,
  // },
};

async function populateFeed(
  source: string,
  extractor: Extractor
): Promise<string> {
  const doc = await fetchFeed(source);
  const feed = new Feed({
    title: doc.title || "",
    description: doc.description || undefined,
    id: doc.link!,
    link: doc.link,
    language: "en",
    copyright: "",
    updated: doc.updated,
    generator: "feed",
  });

  const promises = doc.items.map((item, _i) =>
    (async () => {
      try {
        if (!item.link) {
          throw Error("no link for feed item");
        }
        const html = await extractor(item.link);
        if (!html) {
          throw Error("failed to extract article");
        }
        feed.addItem({
          title: item.title || "",
          date: item.pubDate || new Date(),
          link: item.link,
          description: item.description,
          content: html,
        });
      } catch (e) {
        throw Error("failed to populate feeds for: " + item.link, { cause: e });
      }
    })()
  );
  await Promise.all(promises);
  return feed.atom1();
}

async function scheduled(_event: Event, env: Env, _ctx: ExecutionContext) {
  const errors = new Map<string, unknown>();
  const promises = Object.keys(feeds).map((route) =>
    (async () => {
      const { source, extractor } = feeds[route];
      try {
        const res = await populateFeed(source, extractor);
        await env.BUCKET.put(route, res, {
          httpMetadata: { contentType: "application/xml" },
        });
        errors.set(route, null);
      } catch (e: unknown) {
        console.error(e);
        if (e instanceof Error) {
          errors.set(route, e.message)
        } else {
          errors.set(route, `${e}`);
        }
      }
    })()
  );

  try {
    await Promise.allSettled(promises);
  } catch {}

  const statusPage = renderStatusPage(errors);
  await env.BUCKET.put("index.html", statusPage, {
    httpMetadata: {
      contentType: "text/html",
    },
  });
}

async function handler(req: Request, env: Env): Promise<Response> {
  const url = new URL(req.url);
  if (url.pathname === "/") {
    url.pathname = "/index.html";
    return Response.redirect(url.toString());
  }
  const filepath = url.pathname.startsWith("/")
    ? url.pathname.slice(1, url.pathname.length)
    : url.pathname;
  const obj = await env.BUCKET.get(filepath);
  if (!obj) {
    return new Response(null, { status: 404 });
  }
  return new Response(obj.body, {
    headers: {
      "content-type": obj.httpMetadata?.contentType ?? "text/plain",
      "access-control-allow-origin": "*",
      "access-control-allow-methods": "GET, OPTIONS",
    },
  });
}

async function fetchFeed(url: string) {
  const resp = await fetch(url);
  const text = await resp.text();
  const feed = parseFeed(text);
  if (!feed) {
    throw Error("parse error");
  }
  return feed;
}

function selectElement(html: string, selector: string): string | null {
  const doc = load(html);
  return doc(selector).html();
}

const fakeAgent = {
  body: null,
  method: "GET",
  redirect: "follow",
  headers: {
    accept:
      "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
    "accept-language": "en-US,en;q=0.9",
    "sec-ch-ua":
      '"Google Chrome";v="107", "Chromium";v="107", "Not=A?Brand";v="24"',
  },
} as const;
