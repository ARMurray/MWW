const stripe = require('stripe')(process.env.STRIPE_SECRET_KEY);
const { createClient } = require('@supabase/supabase-js');

const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_SERVICE_ROLE_KEY
);

exports.handler = async (event) => {
  // Only allow POST
  if (event.httpMethod !== 'POST') {
    return { statusCode: 405, body: 'Method Not Allowed' };
  }

  try {
    const { items } = JSON.parse(event.body);

    if (!items || items.length === 0) {
      return { statusCode: 400, body: JSON.stringify({ error: 'Cart is empty' }) };
    }

    // ── Validate stock + reserve each item ──────────────────
    const now = new Date();
    const reserveUntil = new Date(now.getTime() + 15 * 60 * 1000).toISOString(); // 15 min

    for (const item of items) {
      // Check product is still available
      const { data: product, error } = await supabase
        .from('products')
        .select('id, name, stock, reserved_until, stripe_price_id')
        .eq('id', item.id)
        .eq('active', true)
        .single();

      if (error || !product) {
        return {
          statusCode: 400,
          body: JSON.stringify({ error: `Product not found: ${item.name}` })
        };
      }

      // Check stock
      if (product.stock < 1) {
        return {
          statusCode: 400,
          body: JSON.stringify({ error: `Sorry, "${item.name}" is no longer available.` })
        };
      }

      // Check reservation (if reserved by someone else and not expired)
      if (product.reserved_until && new Date(product.reserved_until) > now) {
        return {
          statusCode: 400,
          body: JSON.stringify({ error: `Sorry, "${item.name}" is currently reserved by another customer. Please try again in a few minutes.` })
        };
      }

      // Reserve the item
      await supabase
        .from('products')
        .update({ reserved_until: reserveUntil })
        .eq('id', item.id);
    }

    // ── Build Stripe line items ──────────────────────────────
    const lineItems = items.map(item => ({
      price: item.stripe_price_id,
      quantity: 1
    }));

    // ── Create Stripe Checkout Session ───────────────────────
    const session = await stripe.checkout.sessions.create({
      payment_method_types: ['card'],
      line_items: lineItems,
      mode: 'payment',
      shipping_address_collection: {
        allowed_countries: ['US']
      },
      shipping_options: [
        {
          shipping_rate_data: {
            type: 'fixed_amount',
            fixed_amount: { amount: 1500, currency: 'usd' },
            display_name: 'Standard Shipping',
            delivery_estimate: {
              minimum: { unit: 'business_day', value: 5 },
              maximum: { unit: 'business_day', value: 10 }
            }
          }
        },
        {
          shipping_rate_data: {
            type: 'fixed_amount',
            fixed_amount: { amount: 0, currency: 'usd' },
            display_name: 'Local Pickup (Columbus, OH)',
          }
        }
      ],
      metadata: {
        product_ids: items.map(i => i.id).join(',')
      },
      success_url: `${process.env.URL}/checkout-success`,
      cancel_url: `${process.env.URL}/shop`
    });

    return {
      statusCode: 200,
      body: JSON.stringify({ url: session.url })
    };

  } catch (err) {
    console.error('Checkout error:', err);
    return {
      statusCode: 500,
      body: JSON.stringify({ error: 'Internal server error' })
    };
  }
};